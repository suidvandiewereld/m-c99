-- | Rendering diagnostics.
--
-- The shape is rustc's, by way of the sibling Mettle compiler's
-- src/error/error_reporter.c:
--
-- @
-- error[E0102]: use of undeclared identifier 'coutner'
--   --> hello.c:6:12
--    |
--  5 | int main(void) {
--  6 |     return coutner + 1;
--    |            ^^^^^^^ not found in this scope
--  7 | }
--    |
--    = help: did you mean 'counter'?
-- @
--
-- Deliberately ASCII: @-->@, @|@, @^@, @=@. Box-drawing characters render as
-- mojibake on a stock Windows console, and this compiler is built there.
--
-- Snippets are read from disk at render time rather than carried through the
-- passes, because a location names the original header it came from, not the
-- preprocessed text the lexer actually saw. One cache per run.
module C99.Diag
  ( DiagOptions (..)
  , defaultDiagOptions
  , DiagSink
  , newDiagSink
  , sinkReport
  , sinkCounts
  , sinkFailed
  , sinkSummary
  , renderMessage
  , applyWarnPolicy
  , tidy
  , editDistance
  , closestCandidate
  ) where

import C99.Common
import Control.Exception (IOException, catch)
import Control.Monad (forM_, unless)
import Data.Char (isAlphaNum, toLower)
import Data.IORef
import Data.List (intercalate, minimumBy, sortOn)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Ord (comparing)
import System.IO

data DiagOptions = DiagOptions
  { doColor :: !Bool
  , -- | Groups switched off with @-Wno-...@.
    doSuppressed :: [WarnGroup]
  , -- | @-Werror@: warnings count as errors and fail the build.
    doWarnAsError :: !Bool
  , -- | Stop printing after this many errors. Announced, not silent.
    doMaxErrors :: !Int
  , -- | @--error-format=json@: one JSON object per line, for editors.
    doJson :: !Bool
  , -- | Lines of context above and below the underlined line.
    doContext :: !Int
  }

defaultDiagOptions :: DiagOptions
defaultDiagOptions =
  DiagOptions
    { doColor = False
    , doSuppressed = []
    , doWarnAsError = False
    , doMaxErrors = 100
    , doJson = False
    , doContext = 1
    }

-- ---- colors ----

-- | Follows the usual precedence: CLICOLOR_FORCE beats NO_COLOR beats
-- TERM=dumb beats isatty. Callers resolve this once and store it in
-- 'doColor'; see 'Main.resolveColor'.
red, yellow, cyan, bold, reset :: String
red = "\ESC[31;1m"
yellow = "\ESC[33;1m"
cyan = "\ESC[36;1m"
bold = "\ESC[1m"
reset = "\ESC[0m"

sevColor :: Severity -> String
sevColor Error = red
sevColor Warning = yellow
sevColor Note = cyan

sevLabel :: Severity -> String
sevLabel Error = "error"
sevLabel Warning = "warning"
sevLabel Note = "note"

paint :: Bool -> String -> String -> String
paint False _ s = s
paint True c s = c ++ s ++ reset

-- ---- the sink ----

-- | Holds the source cache and the running counts, so a whole run reports
-- through one object and the summary knows what it saw.
data DiagSink = DiagSink
  { dsOpts :: DiagOptions
  , dsFiles :: IORef [(FilePath, [String])]
  , dsErrors :: IORef Int
  , dsWarnings :: IORef Int
  , dsCapped :: IORef Bool
  }

newDiagSink :: DiagOptions -> IO DiagSink
newDiagSink o =
  DiagSink o
    <$> newIORef []
    <*> newIORef 0
    <*> newIORef 0
    <*> newIORef False

sinkCounts :: DiagSink -> IO (Int, Int)
sinkCounts s = (,) <$> readIORef (dsErrors s) <*> readIORef (dsWarnings s)

-- | Whether anything reported so far should stop the build.
--
-- Ask this rather than 'hasErrors' on a pass's own messages: under @-Werror@
-- a warning becomes an error inside 'sinkReport', and the pass that produced
-- it has no idea that happened.
sinkFailed :: DiagSink -> IO Bool
sinkFailed s = (> 0) <$> readIORef (dsErrors s)

-- | Read a file once and keep its lines. A miss (a header that moved, or the
-- synthetic @\<c99m-u128-runtime\>@ unit) yields no snippet rather than an error:
-- a diagnostic that cannot show source is still worth printing.
sourceLines :: DiagSink -> FilePath -> IO [String]
sourceLines s path = do
  cached <- readIORef (dsFiles s)
  case lookup path cached of
    Just ls -> pure ls
    Nothing -> do
      r <- catchIO (Just . lines <$> readFileLenient path) (pure Nothing)
      let ls = fromMaybe [] r
      modifyIORef' (dsFiles s) ((path, ls) :)
      pure ls

-- | Read forgivingly: a stray byte in a latin-1 comment should not stop us
-- from showing the line someone needs to see.
readFileLenient :: FilePath -> IO String
readFileLenient p = do
  h <- openFile p ReadMode
  enc <- mkTextEncoding "UTF-8//ROUNDTRIP"
  hSetEncoding h enc
  hSetNewlineMode h universalNewlineMode
  c <- hGetContents h
  length c `seq` hClose h
  pure c

catchIO :: IO a -> IO a -> IO a
catchIO act fallback = act `catch` \e -> let _ = (e :: IOException) in fallback

-- ---- warning policy ----

-- | Drop suppressed warnings, and promote what is left under @-Werror@.
applyWarnPolicy :: DiagOptions -> [Message] -> [Message]
applyWarnPolicy o = mapMaybe step
  where
    step m
      | msgSeverity m == Warning
      , Just g <- msgGroup m
      , g `elem` doSuppressed o =
          Nothing
      | msgSeverity m == Warning
      , doWarnAsError o =
          Just m {msgSeverity = Error, msgText = msgText m}
      | otherwise = Just m

-- ---- cascade suppression, ordering, capping ----

-- | Sort by position, then drop what a reader would read as one mistake told
-- twice: an exact repeat at a location, or a second parse error there. Notes
-- ride along with their parent and are never dropped on their own.
tidy :: [Message] -> [Message]
tidy = go [] . sortOn key
  where
    key m = (locFile (msgLoc m), locLine (msgLoc m), locCol (msgLoc m))

    go _ [] = []
    go seen (m : ms)
      | dup = go seen ms
      | otherwise = m : go ((msgLoc m, msgText m) : seen) ms
      where
        dup = any (\(l, t) -> l == msgLoc m && t == msgText m) seen

-- ---- rendering ----

-- | One diagnostic, including its notes, as a block of lines.
renderMessage :: DiagSink -> Message -> IO [String]
renderMessage s m = do
  primary <- frame s m True
  notes <- mapM (\n -> frame s n False) (msgNotes m)
  let helpLine = case msgHelp m of
        Nothing -> []
        Just h ->
          [ gutterPad s m ++ " = " ++ paint (col s) cyan "help" ++ ": " ++ h
          ]
  pure (primary ++ concat notes ++ helpLine)
  where
    col = doColor . dsOpts

-- | Header, location, and the source frame for one message.
frame :: DiagSink -> Message -> Bool -> IO [String]
frame s m isPrimary = do
  ls <- sourceLines s (locFile loc)
  let header =
        paint c (sevColor sev) (sevLabel sev ++ codePart)
          ++ paint c bold (": " ++ msgText m)
      arrow = replicate (gw + 1) ' ' ++ "--> " ++ locFile loc ++ ":" ++ show ln ++ ":" ++ show cl
      body
        | null ls || ln <= 0 || ln > length ls = []
        | otherwise = bar : contextAbove ++ [srcLine ln] ++ [caretLine] ++ contextBelow
      contextAbove = map srcLine [max 1 (ln - ctx) .. ln - 1]
      contextBelow = map srcLine [ln + 1 .. min (length ls) (ln + ctx)]
      srcLine n = padNum n ++ " | " ++ expandTabs (ls !! (n - 1))
      bar = replicate (gw + 1) ' ' ++ "|"
      caretLine =
        replicate (gw + 1) ' '
          ++ "| "
          ++ replicate caretCol ' '
          ++ paint c (sevColor sev) (replicate caretLen '^' ++ labelPart)
      labelPart = case msgLabel m of
        Nothing -> ""
        Just l -> " " ++ l
      -- Tabs are expanded in the shown line, so the caret has to move by the
      -- same amount or it points at the wrong character.
      caretCol = displayWidth (take (max 0 (snapCol - 1)) rawLine)
      caretLen = max 1 snapLen
      (snapCol, snapLen) = snap rawLine (cl, msgLen m) (msgSnap m)
      rawLine = if ln > 0 && ln <= length ls then ls !! (ln - 1) else ""
      padNum n = let t = show n in replicate (gw - length t) ' ' ++ t
  pure (header : arrow : body)
  where
    c = doColor (dsOpts s)
    ctx = doContext (dsOpts s)
    sev = msgSeverity m
    loc = msgLoc m
    ln = locLine loc
    cl = locCol loc
    gw = gutterWidth m (doContext (dsOpts s))
    codePart
      | isPrimary, Just code <- msgCode m = "[" ++ code ++ "]"
      | otherwise = ""

-- | Move the caret onto a named identifier.
--
-- A declaration's location is the start of its type, so @int scratch@ reported
-- at the declaration underlines @int scr@. Rather than correct the location in
-- every pass that builds one, look for the name on the line as a whole word.
-- Falls back to the given span when the name is not there, which is what
-- happens on a macro-expanded line.
snap :: String -> (Int, Int) -> Maybe String -> (Int, Int)
snap _ given Nothing = given
snap line given@(_, _) (Just name)
  | null name = given
  | otherwise = case wholeWordAt 0 line of
      Just i -> (i + 1, length name)
      Nothing -> given
  where
    wholeWordAt _ [] = Nothing
    wholeWordAt i s@(_ : r)
      | name `isPrefixOfStr` s
      , leftOk i
      , rightOk (drop (length name) s) =
          Just i
      | otherwise = wholeWordAt (i + 1) r

    leftOk 0 = True
    leftOk i = not (identChar (line !! (i - 1)))

    rightOk [] = True
    rightOk (ch : _) = not (identChar ch)

    identChar ch = isAlphaNum ch || ch == '_'

isPrefixOfStr :: String -> String -> Bool
isPrefixOfStr [] _ = True
isPrefixOfStr _ [] = False
isPrefixOfStr (a : as) (b : bs) = a == b && isPrefixOfStr as bs

-- | Gutter is as wide as the largest line number it will print, so the bars
-- line up without a file-wide pre-pass.
gutterWidth :: Message -> Int -> Int
gutterWidth m ctx = length (show (max 1 (locLine (msgLoc m) + ctx)))

gutterPad :: DiagSink -> Message -> String
gutterPad s m = replicate (gutterWidth m (doContext (dsOpts s)) + 1) ' '

-- | A tab advances to the next multiple of 8, the width every terminal and
-- editor agrees on. Mettle's renderer skips this and misaligns on tabbed
-- source; C code is full of tabs, so it matters here.
expandTabs :: String -> String
expandTabs = go 0
  where
    go _ [] = []
    go n ('\t' : r) = let w = 8 - (n `mod` 8) in replicate w ' ' ++ go (n + w) r
    go n (ch : r) = ch : go (n + 1) r

displayWidth :: String -> Int
displayWidth = go 0
  where
    go n [] = n
    go n ('\t' : r) = go (n + (8 - (n `mod` 8))) r
    go n (_ : r) = go (n + 1) r

-- ---- reporting ----

-- | Print a pass's messages. Applies the warning policy, orders and
-- deduplicates, then stops at the error cap and says so.
sinkReport :: DiagSink -> [Message] -> IO ()
sinkReport s msgs0 = do
  let msgs = tidy (applyWarnPolicy (dsOpts s) msgs0)
  forM_ msgs $ \m -> do
    (es, _) <- sinkCounts s
    capped <- readIORef (dsCapped s)
    let over = es >= doMaxErrors (dsOpts s)
    if over
      then
        unless capped $ do
          writeIORef (dsCapped s) True
          hPutStrLn stderr $
            paint (doColor (dsOpts s)) red "error"
              ++ ": too many errors emitted, stopping now"
          hPutStrLn stderr $
            "  = help: raise the limit with --max-errors=N"
      else do
        case msgSeverity m of
          Error -> modifyIORef' (dsErrors s) (+ 1)
          Warning -> modifyIORef' (dsWarnings s) (+ 1)
          Note -> pure ()
        out <-
          if doJson (dsOpts s)
            then pure [jsonMessage m]
            else renderMessage s m
        mapM_ (hPutStrLn stderr) out
        unless (doJson (dsOpts s)) (hPutStrLn stderr "")

-- | The closing line, in cargo's words: what failed and how much.
sinkSummary :: DiagSink -> String -> IO ()
sinkSummary s what = do
  (es, ws) <- sinkCounts s
  unless (doJson (dsOpts s) || (es == 0 && ws == 0)) $ do
    let plural n x = show n ++ " " ++ x ++ (if n == 1 then "" else "s")
    if es > 0
      then
        hPutStrLn stderr $
          paint (doColor (dsOpts s)) red "error"
            ++ ": could not compile `"
            ++ what
            ++ "` due to "
            ++ plural es "previous error"
            ++ (if ws > 0 then "; " ++ plural ws "warning" ++ " emitted" else "")
      else
        hPutStrLn stderr $
          paint (doColor (dsOpts s)) yellow "warning"
            ++ ": "
            ++ plural ws "warning"
            ++ " emitted"

-- ---- json ----

jsonMessage :: Message -> String
jsonMessage m =
  "{"
    ++ intercalate
      ","
      ( [ str "severity" (sevLabel (msgSeverity m))
        , str "file" (locFile (msgLoc m))
        , num "line" (locLine (msgLoc m))
        , num "column" (locCol (msgLoc m))
        , num "length" (max 1 (msgLen m))
        , str "message" (msgText m)
        ]
          ++ maybe [] (\c -> [str "code" c]) (msgCode m)
          ++ maybe [] (\l -> [str "label" l]) (msgLabel m)
          ++ maybe [] (\h -> [str "help" h]) (msgHelp m)
          ++ maybe [] (\g -> [str "group" ("-W" ++ warnGroupName g)]) (msgGroup m)
          ++ [ "\"notes\":["
                 ++ intercalate "," (map jsonMessage (msgNotes m))
                 ++ "]"
             | not (null (msgNotes m))
             ]
      )
    ++ "}"
  where
    str k v = "\"" ++ k ++ "\":\"" ++ esc v ++ "\""
    num k v = "\"" ++ k ++ "\":" ++ show v
    esc = concatMap e
    e '"' = "\\\""
    e '\\' = "\\\\"
    e '\n' = "\\n"
    e '\r' = "\\r"
    e '\t' = "\\t"
    e ch
      | ch < ' ' = "\\u" ++ pad (hex (fromEnum ch))
      | otherwise = [ch]
    pad h = replicate (4 - length h) '0' ++ h
    hex 0 = "0"
    hex n = go n ""
      where
        go 0 acc = acc
        go k acc = go (k `div` 16) (("0123456789abcdef" !! (k `mod` 16)) : acc)

-- ---- did you mean ----

-- | Levenshtein, case-insensitive so @Print@ and @print@ come out at 0.
-- One rolling row: the table is only ever read one line behind.
editDistance :: String -> String -> Int
editDistance a0 b0 = last (foldl step [0 .. length s] t)
  where
    s = map toLower a0
    t = map toLower b0
    -- 'prev' is the previous row; 'z' the cell to the left, 'diag' the one up
    -- and left, 'up' the one directly above.
    step prev@(p : ps) c = scanl cell (p + 1) (zip3 s prev ps)
      where
        cell z (sc, upLeft, up) =
          minimum [up + 1, z + 1, upLeft + (if c == sc then 0 else 1)]
    step [] _ = []

-- | The nearest name worth suggesting, or nothing. The threshold scales with
-- the name's length: one edit for short names, never more than three, so a
-- long unrelated identifier does not get proposed. Exact matches are skipped,
-- since suggesting the name someone already wrote helps nobody.
closestCandidate :: String -> [String] -> Maybe String
closestCandidate name cands
  | null scored = Nothing
  | otherwise = Just (fst (minimumBy (comparing snd) scored))
  where
    threshold = max 1 (min 3 (length name `div` 3))
    scored =
      [ (c, d)
      | c <- cands
      , let d = editDistance name c
      , d > 0
      , d <= threshold
      ]
