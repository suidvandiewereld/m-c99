-- | The C preprocessor (src/preprocess.c).
--
-- Text in, text out: the result is a single translation unit with @# n "file"@
-- line markers so the lexer can attribute tokens to the header they came from.
module C99.Preprocess
  ( PPOptions (..)
  , PPCache
  , newPPCache
  , defaultPPOptions
  , preprocess
  ) where

import C99.Common (Message (..), Severity (..), SrcLoc (..), diag)
import Control.Exception (IOException, try)
import Control.Monad.State.Strict
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Char8 (ByteString)
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import Data.Char (digitToInt, isDigit, isHexDigit, isOctDigit, ord)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import Data.Int (Int64)
import Data.List (intercalate, isPrefixOf)
import qualified Data.Map.Strict as M
import System.Directory (doesFileExist)

data PPOptions = PPOptions
  { ppIncludeDirs :: [FilePath]
  , ppDefines :: [(String, String)]
  , -- | Share one cache (see 'newPPCache') across the translation units of a
    -- run and every header is read and resolved once instead of once per TU.
    -- 'Nothing' disables caching. A cache assumes the include dirs it serves
    -- are fixed, so do not share one between differently-configured options.
    ppCache :: Maybe PPCache
  }

defaultPPOptions :: PPOptions
defaultPPOptions = PPOptions {ppIncludeDirs = [], ppDefines = [], ppCache = Nothing}

-- | File contents and include resolutions, keyed for reuse across TUs.
data PPCache = PPCache
  { pcFiles :: IORef (M.Map FilePath (Maybe String))
  , pcResolve :: IORef (M.Map (FilePath, Bool, String) (Maybe FilePath))
  , -- | 'phase12' + 'physLines' output. A pure function of the file content,
    -- so it can be shared across the many TUs that include the same header.
    pcLines :: IORef (M.Map FilePath [String])
  }

newPPCache :: IO PPCache
newPPCache = PPCache <$> newIORef M.empty <*> newIORef M.empty <*> newIORef M.empty

-- ---- macro table ----

data Macro = Macro
  { mName :: String
  , mFunc :: !Bool
  , mVariadic :: !Bool
  , mParams :: [String]
  , mBody :: String
  , mPredef :: !Bool
  }

-- | User definitions in one map, predefines in another, user shadowing predef
-- on lookup. Header-sized macro tables are consulted for every identifier of
-- every line, so this must not be a list.
data Table = Table
  { tabUser :: !(M.Map String Macro)
  , tabPre :: !(M.Map String Macro)
  }

emptyTable :: Table
emptyTable = Table M.empty M.empty

macroFind :: Table -> String -> Maybe Macro
macroFind tbl n = case M.lookup n (tabUser tbl) of
  hit@(Just _) -> hit
  Nothing -> M.lookup n (tabPre tbl)

-- | Redefining a predefined macro shadows it rather than overwriting it, so it
-- survives a later @#undef@ (which likewise refuses to remove predefines).
macroDefine :: Macro -> Table -> Table
macroDefine m tbl
  | mPredef m = tbl {tabPre = M.insert (mName m) m (tabPre tbl)}
  | otherwise = tbl {tabUser = M.insert (mName m) m (tabUser tbl)}

macroUndef :: String -> Table -> Table
macroUndef n tbl = tbl {tabUser = M.delete n (tabUser tbl)}

objMacro :: Bool -> String -> String -> Macro
objMacro predef n body =
  Macro
    { mName = n
    , mFunc = False
    , mVariadic = False
    , mParams = []
    , mBody = body
    , mPredef = predef
    }

-- ---- character classes ----

isIdentStart :: Char -> Bool
isIdentStart c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'

isIdentCont :: Char -> Bool
isIdentCont c = isIdentStart c || (c >= '0' && c <= '9')

isSpaceTab :: Char -> Bool
isSpaceTab c = c == ' ' || c == '\t'

-- | Split off a leading identifier; yields 'Nothing' when the text does not
-- start with one (and then consumes nothing).
readIdent :: String -> (Maybe String, String)
readIdent s@(c : _)
  | isIdentStart c = let (i, r) = span isIdentCont s in (Just i, r)
readIdent s = (Nothing, s)

-- ---- phases 1-2: trigraphs and line splicing ----

-- | Splices delete a physical newline; the deleted newlines are re-emitted once
-- the logical line ends, so physical line numbering survives for diagnostics.
phase12 :: String -> String
phase12 = go (0 :: Int)
  where
    go pending ('?' : '?' : c : r)
      | Just rep <- trigraph c = rep : go pending r
    go pending ('\\' : '\n' : r) = go (pending + 1) r
    go pending ('\\' : '\r' : '\n' : r) = go (pending + 1) r
    -- CRLF becomes LF here, so 'physLines' need not strip anything per line
    go pending ('\r' : '\n' : r) = go pending ('\n' : r)
    go pending ('\n' : r) = '\n' : replicate pending '\n' ++ go 0 r
    go pending (c : r) = c : go pending r
    go pending [] = replicate pending '\n'

    trigraph c = case c of
      '=' -> Just '#'
      '/' -> Just '\\'
      '\'' -> Just '^'
      '(' -> Just '['
      ')' -> Just ']'
      '!' -> Just '|'
      '<' -> Just '{'
      '>' -> Just '}'
      '-' -> Just '~'
      _ -> Nothing

stripBom :: String -> String
stripBom s
  | "\xEF\xBB\xBF" `isPrefixOf` s = drop 3 s
  | otherwise = s

-- | Physical lines. CRLF was already normalized to LF by 'phase12', and a
-- final newline does not introduce a trailing empty line.
physLines :: String -> [String]
physLines = lines

-- ---- #if expression evaluation ----

-- Operands reach here already macro-expanded (see 'ifCondition'), so an
-- identifier still standing is one C99 6.10.1p4 makes 0.

type E = (Int64, String)

eSkip :: String -> String
eSkip s = case dropWhile isSpaceTab s of
  ('/' : '/' : r) -> eSkip (dropWhile (/= '\n') r)
  s' -> s'

evalExpr :: Table -> String -> Int64
evalExpr tbl s = fst (eOr tbl s)

eOr :: Table -> String -> E
eOr tbl s0 = loop (eAnd tbl s0)
  where
    loop (v, s) = case eSkip s of
      ('|' : '|' : r) ->
        let (rv, r') = eAnd tbl r
         in loop (if v /= 0 || rv /= 0 then 1 else 0, r')
      s' -> (v, s')

eAnd :: Table -> String -> E
eAnd tbl s0 = loop (eBor tbl s0)
  where
    loop (v, s) = case eSkip s of
      ('&' : '&' : r) ->
        let (rv, r') = eBor tbl r
         in loop (if v /= 0 && rv /= 0 then 1 else 0, r')
      s' -> (v, s')

eBor :: Table -> String -> E
eBor tbl s0 = loop (eBxor tbl s0)
  where
    loop (v, s) = case eSkip s of
      ('|' : c : r) | c /= '|' -> let (rv, r') = eBxor tbl (c : r) in loop (v .|. rv, r')
      ['|'] -> let (rv, r') = eBxor tbl "" in loop (v .|. rv, r')
      s' -> (v, s')

eBxor :: Table -> String -> E
eBxor tbl s0 = loop (eBand tbl s0)
  where
    loop (v, s) = case eSkip s of
      ('^' : r) -> let (rv, r') = eBand tbl r in loop (v `xor` rv, r')
      s' -> (v, s')

eBand :: Table -> String -> E
eBand tbl s0 = loop (eEq tbl s0)
  where
    loop (v, s) = case eSkip s of
      ('&' : c : r) | c /= '&' -> let (rv, r') = eEq tbl (c : r) in loop (v .&. rv, r')
      ['&'] -> let (rv, r') = eEq tbl "" in loop (v .&. rv, r')
      s' -> (v, s')

eEq :: Table -> String -> E
eEq tbl s0 = loop (eRel tbl s0)
  where
    loop (v, s) = case eSkip s of
      ('=' : '=' : r) -> let (rv, r') = eRel tbl r in loop (bool (v == rv), r')
      ('!' : '=' : r) -> let (rv, r') = eRel tbl r in loop (bool (v /= rv), r')
      s' -> (v, s')

eRel :: Table -> String -> E
eRel tbl s0 = loop (eShift tbl s0)
  where
    loop (v, s) = case eSkip s of
      ('<' : '=' : r) -> let (rv, r') = eShift tbl r in loop (bool (v <= rv), r')
      ('>' : '=' : r) -> let (rv, r') = eShift tbl r in loop (bool (v >= rv), r')
      ('<' : c : r) | c /= '<' -> let (rv, r') = eShift tbl (c : r) in loop (bool (v < rv), r')
      ['<'] -> let (rv, r') = eShift tbl "" in loop (bool (v < rv), r')
      ('>' : c : r) | c /= '>' -> let (rv, r') = eShift tbl (c : r) in loop (bool (v > rv), r')
      ['>'] -> let (rv, r') = eShift tbl "" in loop (bool (v > rv), r')
      s' -> (v, s')

eShift :: Table -> String -> E
eShift tbl s0 = loop (eAdd tbl s0)
  where
    loop (v, s) = case eSkip s of
      ('<' : '<' : r) -> let (rv, r') = eAdd tbl r in loop (v `shiftL` clamp rv, r')
      ('>' : '>' : r) -> let (rv, r') = eAdd tbl r in loop (v `shiftR` clamp rv, r')
      s' -> (v, s')
    clamp n
      | n < 0 = 0
      | n > 64 = 64
      | otherwise = fromIntegral n

eAdd :: Table -> String -> E
eAdd tbl s0 = loop (eMul tbl s0)
  where
    loop (v, s) = case eSkip s of
      ('+' : r) -> let (rv, r') = eMul tbl r in loop (v + rv, r')
      ('-' : r) -> let (rv, r') = eMul tbl r in loop (v - rv, r')
      s' -> (v, s')

eMul :: Table -> String -> E
eMul tbl s0 = loop (ePrimary tbl s0)
  where
    loop (v, s) = case eSkip s of
      ('*' : r) -> let (rv, r') = ePrimary tbl r in loop (v * rv, r')
      ('/' : r) -> let (rv, r') = ePrimary tbl r in loop (if rv /= 0 then v `quot` rv else 0, r')
      ('%' : r) -> let (rv, r') = ePrimary tbl r in loop (if rv /= 0 then v `rem` rv else 0, r')
      s' -> (v, s')

ePrimary :: Table -> String -> E
ePrimary tbl s0 = case eSkip s0 of
  [] -> (0, [])
  ('(' : r) ->
    let (v, r') = eOr tbl r
     in (v, dropChar ')' (eSkip r'))
  ('!' : r) -> let (v, r') = ePrimary tbl r in (bool (v == 0), r')
  ('~' : r) -> let (v, r') = ePrimary tbl r in (complement v, r')
  ('+' : r) -> ePrimary tbl r
  ('-' : r) -> let (v, r') = ePrimary tbl r in (negate v, r')
  ('\'' : r) -> charConst r
  s@(c : _)
    | isIdentStart c ->
        let (name, r) = span isIdentCont s
         in if name == "defined" then definedOp r else (macroValue name, r)
    | isDigit c ->
        case cStrtoll s of
          Just (v, r) -> (v, dropWhile (`elem` "uUlL") r)
          Nothing -> (0, s)
    | otherwise -> (0, s)
  where
    dropChar c (x : r) | x == c = r
    dropChar _ s = s

    definedOp r0 =
      let (paren, r1) = case eSkip r0 of
            ('(' : r) -> (True, eSkip r)
            r -> (False, r)
          (mid, r2) = readIdent r1
          r3 = if paren then dropChar ')' (eSkip r2) else r2
          found = case mid of
            Just i -> maybe False (const True) (macroFind tbl i)
            Nothing -> False
       in (bool found, r3)

    macroValue name = case macroFind tbl name of
      Just m
        | not (mFunc m)
        , Just (v, _) <- cStrtoll (mBody m) ->
            v
      _ -> 0

    charConst ('\\' : c : r) = (esc c, dropChar '\'' r)
    charConst (c : r) = (fromIntegral (ord c .&. 0xff), dropChar '\'' r)
    charConst [] = (0, [])

    esc 'n' = 10
    esc 't' = 9
    esc '0' = 0
    esc c = fromIntegral (ord c .&. 0xff)

bool :: Bool -> Int64
bool True = 1
bool False = 0

-- | @strtoll(s, &end, 0)@: 'Nothing' when no digits were consumed.
cStrtoll :: String -> Maybe (Int64, String)
cStrtoll s0 =
  let s1 = dropWhile (`elem` " \t\n\r\f\v") s0
      (neg, s2) = case s1 of
        ('-' : r) -> (True, r)
        ('+' : r) -> (False, r)
        _ -> (False, s1)
   in case s2 of
        ('0' : x : r)
          | x == 'x' || x == 'X'
          , (d : _) <- r
          , isHexDigit d ->
              Just (digits 16 neg (span isHexDigit r))
        ('0' : r) -> Just (digits 8 neg (span isOctDigit r))
        _ -> case span isDigit s2 of
          ("", _) -> Nothing
          spl -> Just (digits 10 neg spl)
  where
    digits base neg (ds, r) =
      let v = foldl' (\a c -> a * base + fromIntegral (digitToInt c)) 0 ds
       in (if neg then negate v else v, r)

-- | @strtol(s, &end, 10)@ on a non-negative literal.
readDec :: String -> (Int, String)
readDec s = case span isDigit s of
  ("", r) -> (0, r)
  (ds, r) -> (foldl' (\a c -> a * 10 + digitToInt c) 0 ds, r)

-- ---- lexical scanners shared by expansion and paren tracking ----

-- | Consume a string/char literal body, including the closing quote.
scanLit :: Char -> String -> (String, String)
scanLit _ [] = ("", [])
scanLit q ('\\' : c : r) = let (a, r') = scanLit q r in ('\\' : c : a, r')
scanLit _ ['\\'] = ("\\", [])
scanLit q (c : r)
  | c == q = ([c], r)
  | otherwise = let (a, r') = scanLit q r in (c : a, r')

-- | Phase 3: each comment becomes one space.
--
-- Ordinary text keeps its comments, because the lexer strips them and @-E@
-- output reads better with them. But a directive captures raw text that no
-- lexer ever sees: a @#define@ body would carry @// count@ into every
-- expansion and comment out the rest of the line, and a @#if@ operand would
-- feed @/* why */@ to the expression parser, which reads the @/@ as division.
stripComments :: String -> String
stripComments = go
  where
    go [] = []
    -- a line comment runs to the end of the logical line
    go ('/' : '/' : _) = " "
    go ('/' : '*' : r) = ' ' : go (snd (scanBlockComment r))
    go (q : r)
      | q == '"' || q == '\'' = let (lit, r') = scanLit q r in q : lit ++ go r'
    go (c : r) = c : go r

-- | Consume a block comment body, including the closing @*/@. Fewer than two
-- characters left means the comment is unterminated and the tail is left alone.
scanBlockComment :: String -> (String, String)
scanBlockComment ('*' : '/' : r) = ("*/", r)
scanBlockComment (c : r@(_ : _)) = let (a, r') = scanBlockComment r in (c : a, r')
scanBlockComment s = ("", s)

-- | As 'scanBlockComment', but for a line that *starts* inside the comment: an
-- unterminated comment swallows the whole line.
scanOpenComment :: String -> (String, String)
scanOpenComment ('*' : '/' : r) = ("*/", r)
scanOpenComment (c : r) = let (a, r') = scanOpenComment r in (c : a, r')
scanOpenComment [] = ("", [])

-- | Paren depth of one physical line, ignoring strings, chars and comments.
-- Returns the updated depth, the block-comment state to carry to the next line,
-- and the offset of a @//@ comment if one starts on this line.
lineParenDepth :: String -> Int -> Bool -> (Int, Bool, Maybe Int)
lineParenDepth s depth0 inC0 = go (0 :: Int) s depth0 inC0
  where
    go k str d True = case str of
      ('*' : '/' : r) -> go (k + 2) r d False
      (_ : r) -> go (k + 1) r d True
      [] -> (d, True, Nothing)
    go k str d False = case str of
      ('/' : '/' : _) -> (d, False, Just k)
      ('/' : '*' : r) -> go (k + 2) r d True
      (q : r)
        | q == '"' || q == '\'' ->
            let (lit, r') = scanLit q r in go (k + 1 + length lit) r' d False
      ('(' : r) -> go (k + 1) r (d + 1) False
      (')' : r) -> go (k + 1) r (d - 1) False
      (_ : r) -> go (k + 1) r d False
      [] -> (d, False, Nothing)

-- ---- expansion ----

stringifyArg :: String -> String
stringifyArg arg = '"' : concatMap esc arg ++ "\""
  where
    esc c
      | c == '\\' || c == '"' = ['\\', c]
      | c == '\r' = ""
      | otherwise = [c]

-- | Expand one logical line. @startInComment@ says the line begins inside a
-- block comment opened on an earlier line.
expandLine :: Table -> FilePath -> Int -> String -> Bool -> String
expandLine tbl path line input startInComment =
  expandLine' [] tbl path line input startInComment

-- | As 'expandLine', but carrying the names whose expansion we are inside.
--
-- C99 6.10.3.4p2: a macro name found while rescanning its own replacement is
-- not replaced again. Without this @#define foo foo@ loops forever rather than
-- terminating, and so does any mutually recursive pair.
expandLine' :: [String] -> Table -> FilePath -> Int -> String -> Bool -> String
expandLine' active tbl path line input startInComment
  -- Comments, strings, and ordinary text pass through verbatim below, so when
  -- no identifier of the line could expand, the whole function is the
  -- identity: return the input unrebuilt. The candidate scan tokenizes the
  -- way `go` does (including inside strings), so it can only over-approximate.
  | not (anyCandidate input) = input
  | otherwise = prefix ++ go body
  where
    anyCandidate [] = False
    anyCandidate s@(c : r)
      | isIdentStart c =
          let (i, r') = span isIdentCont s
           in i == "__FILE__"
                || i == "__LINE__"
                || (case macroFind tbl i of Just _ -> True; Nothing -> False)
                || anyCandidate r'
      | otherwise = anyCandidate r

    (prefix, body)
      | startInComment = scanOpenComment input
      | otherwise = ("", input)

    go [] = []
    go ('/' : '/' : r) = '/' : '/' : r
    go ('/' : '*' : r) = let (c, r') = scanBlockComment r in '/' : '*' : c ++ go r'
    go (q : r)
      | q == '"' || q == '\'' = let (lit, r') = scanLit q r in q : lit ++ go r'
    go s@(c : r)
      | isIdentStart c = let (i, r') = span isIdentCont s in ident i r'
      | otherwise = c : go r

    ident "__FILE__" r = '"' : map fwd path ++ '"' : go r
    ident "__LINE__" r = show line ++ go r
    ident i r
      | i `elem` active = i ++ go r -- painted blue: leave it alone forever
    ident i r = case macroFind tbl i of
      Nothing -> i ++ go r
      Just m
        | not (mFunc m) -> substMacro active tbl path line m [] ++ go r
        | otherwise ->
            case dropWhile isSpaceTab r of
              ('(' : r1) ->
                let (args, r2) = scanArgs r1
                 in substMacro active tbl path line m args ++ go r2
              _ -> i ++ go r

    fwd '\\' = '/'
    fwd c = c

-- | Split a function-like invocation's argument list, having consumed the @(@.
scanArgs :: String -> ([String], String)
scanArgs (')' : r) = ([], r)
scanArgs s0 = collect s0
  where
    collect s =
      let (arg, s') = scanArg s (0 :: Int)
          arg' = dropWhile isSpaceTab arg
       in case s' of
            (',' : r) -> let (as, r') = collect r in (arg' : as, r')
            (')' : r) -> ([arg'], r)
            _ -> ([arg'], s')

    scanArg [] _ = ("", [])
    scanArg s@(c : r) d
      | c == '"' || c == '\'' =
          let (lit, r1) = scanLit c r
              (a, r2) = scanArg r1 d
           in (c : lit ++ a, r2)
      | c == '(' = cons c (scanArg r (d + 1))
      | c == ')' = if d == 0 then ("", s) else cons c (scanArg r (d - 1))
      | c == ',' && d == 0 = ("", s)
      | otherwise = cons c (scanArg r d)

    cons c (a, r) = (c : a, r)

-- | Substitute arguments into a macro body, then rescan the result with this
-- macro's own name held back, so the rescan cannot re-enter it.
substMacro :: [String] -> Table -> FilePath -> Int -> Macro -> [String] -> String
substMacro active tbl path line m args =
  expandLine' (mName m : active) tbl path line (subst [] (mBody m)) False
  where
    nargs = length args
    fixed = length (mParams m)

    paramIndex n = lookup n (zip (mParams m) [0 ..])

    argAt k = args !! k

    -- The accumulator is reversed so `##` can strip the whitespace it follows.
    subst acc [] = reverse acc
    subst acc ('#' : '#' : r) =
      subst (dropWhile isSpaceTab acc) (dropWhile isSpaceTab r)
    subst acc ('#' : r) =
      let r1 = dropWhile isSpaceTab r
       in case readIdent r1 of
            (Just pname, r2) -> case paramIndex pname of
              Just k | k < nargs -> subst (revApp (stringifyArg (argAt k)) acc) r2
              _ -> subst acc r2
            (Nothing, r2) -> subst acc r2
    subst acc s@(c : r)
      | isIdentStart c =
          let (i, r') = span isIdentCont s
           in case paramIndex i of
                Just k | k < nargs -> subst (revApp (argAt k) acc) r'
                _
                  | mVariadic m && i == "__VA_ARGS__" ->
                      subst (revApp (intercalate "," (drop fixed args)) acc) r'
                  | otherwise -> subst (revApp i acc) r'
      | otherwise = subst (c : acc) r

    revApp s acc = foldl' (flip (:)) acc s

-- ---- include search ----

joinPath :: FilePath -> FilePath -> FilePath
joinPath dir file
  | null dir = file
  | last dir == '/' || last dir == '\\' = dir ++ file
  | otherwise = dir ++ "/" ++ file

dirOf :: FilePath -> FilePath
dirOf p = case break (`elem` "/\\") (reverse p) of
  (_, _ : rd) -> reverse rd
  _ -> "."

findInclude :: [FilePath] -> FilePath -> String -> Bool -> IO (Maybe FilePath)
findInclude paths fromPath name angled =
  firstExisting (local ++ map (`joinPath` name) paths ++ [name])
  where
    local
      | angled = []
      | otherwise = [joinPath (dirOf fromPath) name]

    firstExisting [] = return Nothing
    firstExisting (c : cs) = do
      ok <- doesFileExist c
      if ok then return (Just c) else firstExisting cs

-- | 'findInclude' / 'readFileBytes' through the cross-TU cache, when one is
-- present. Headers are included by every TU; without this they are re-probed
-- and re-read from disk once per unit.
cachedResolve :: Maybe PPCache -> [FilePath] -> FilePath -> String -> Bool -> IO (Maybe FilePath)
cachedResolve Nothing paths fromPath name angled = findInclude paths fromPath name angled
cachedResolve (Just c) paths fromPath name angled = do
  let key = (dirOf fromPath, angled, name)
  m <- readIORef (pcResolve c)
  case M.lookup key m of
    Just hit -> return hit
    Nothing -> do
      r <- findInclude paths fromPath name angled
      modifyIORef' (pcResolve c) (M.insert key r)
      return r

cachedPhysLines :: FilePath -> String -> PPM [String]
cachedPhysLines path raw = do
  mc <- gets stCache
  case mc of
    Nothing -> return fresh
    Just c -> do
      m <- liftIO (readIORef (pcLines c))
      case M.lookup path m of
        Just hit -> return hit
        Nothing -> do
          liftIO (modifyIORef' (pcLines c) (M.insert path fresh))
          return fresh
  where
    fresh = physLines (phase12 (stripBom raw))

cachedRead :: Maybe PPCache -> FilePath -> IO (Maybe String)
cachedRead Nothing p = readFileBytes p
cachedRead (Just c) p = do
  m <- readIORef (pcFiles c)
  case M.lookup p m of
    Just hit -> return hit
    Nothing -> do
      r <- readFileBytes p
      modifyIORef' (pcFiles c) (M.insert p r)
      return r

readFileBytes :: FilePath -> IO (Maybe String)
readFileBytes p = do
  r <- try (BS.readFile p) :: IO (Either IOException BS.ByteString)
  return (either (const Nothing) (Just . BS.unpack) r)

-- ---- driver state ----

data St = St
  { stMacros :: Table
  , stIfStack :: [Int] -- innermost first: 0 dead, 1 taking, 2 branch already taken
  , stDisabled :: !Bool
  , stIncludeStack :: [FilePath]
  , stMsgs :: [Message] -- reversed
  , stPaths :: [FilePath]
  , stCache :: Maybe PPCache
  }

type PPM = StateT St IO

ppError :: FilePath -> Int -> String -> PPM ()
ppError path line text =
  modify $ \s -> s {stMsgs = diag Error (SrcLoc path line 1) text : stMsgs s}

-- | Output is suppressed unless every enclosing conditional is in its taken arm.
recompute :: PPM ()
recompute = modify $ \s -> s {stDisabled = any (/= 1) (stIfStack s)}

-- | A single newline chunk. The preprocessor emits blank lines constantly to
-- keep physical line numbering, so this shared 'ByteString' saves packing one
-- for every splice and every disabled line.
nlB :: ByteString
nlB = BS.singleton '\n'

lineMarker :: Int -> FilePath -> ByteString
lineMarker line path = BS.pack ("# " ++ show line ++ " \"" ++ map fwd path ++ "\"\n")
  where
    fwd '\\' = '/'
    fwd c = c

-- ---- one buffer ----

ppBuffer :: FilePath -> String -> PPM ByteString
ppBuffer path raw = do
  stack <- gets stIncludeStack
  if path `elem` stack
    then do
      ppError path 1 "include cycle"
      return BS.empty
    else do
      modify $ \s -> s {stIncludeStack = path : stIncludeStack s}
      src <- cachedPhysLines path raw
      chunks <- ppLines path src 1 False [lineMarker 1 path]
      modify $ \s -> s {stIncludeStack = drop 1 (stIncludeStack s)}
      return (BS.concat (reverse chunks))

-- | @acc@ holds the emitted chunks in reverse order. Each chunk is a
-- 'ByteString', so the whole translation unit is assembled with a single
-- 'BS.concat' at the end rather than materializing a @[Char]@ list that the
-- lexer would immediately pack straight back into bytes.
ppLines :: FilePath -> [String] -> Int -> Bool -> [ByteString] -> PPM [ByteString]
ppLines _ [] _ _ acc = return acc
ppLines path (raw : rest) line inCmt acc
  | not inCmt, ('#' : _) <- dropWhile isSpaceTab raw = do
      acc' <- directive path raw line acc
      ppLines path rest (line + 1) inCmt acc'
  | otherwise = do
      dis <- gets stDisabled
      if dis
        then ppLines path rest (line + 1) inCmt (nlB : acc)
        else do
          tbl <- gets stMacros
          let (d0, cmt1, cmtAt) = lineParenDepth raw 0 inCmt
              logical0
                | d0 > 0, Just k <- cmtAt = take k raw
                | otherwise = raw
              (logical, merged, inCmt', rest') = mergeLines d0 cmt1 logical0 rest 0
              expanded = expandLine tbl path line logical inCmt
              acc' = nlB : replicate merged nlB ++ (BS.pack expanded : acc)
          ppLines path rest' (line + merged + 1) inCmt' acc'

-- | A function-like invocation may span physical lines: while the line leaves
-- @(@ unbalanced, absorb following lines (directives stop the merge). Blank
-- lines are emitted afterwards to keep the line count.
mergeLines :: Int -> Bool -> String -> [String] -> Int -> (String, Int, Bool, [String])
mergeLines depth inC logical ls merged
  | depth > 0
  , merged < 512
  , (nraw : rest) <- ls
  , not (startsHash nraw) =
      let (d', inC', ncmt) = lineParenDepth nraw depth inC
          nraw' = maybe nraw (`take` nraw) ncmt
       in mergeLines d' inC' (logical ++ " " ++ nraw') rest (merged + 1)
  | otherwise = (logical, merged, inC, ls)
  where
    startsHash s = case dropWhile isSpaceTab s of
      ('#' : _) -> True
      _ -> False

-- ---- directives ----

-- | Evaluate a @#if@ / @#elif@ condition.
--
-- C99 6.10.1p4 orders this: strip comments, resolve @defined X@ (whose operand
-- must survive expansion), macro-expand what is left, then evaluate. Skipping
-- the expansion step made @#define VER (1 << 8)@ followed by @#if VER > 100@
-- take the false branch, because a body that is not a bare integer read as 0.
ifCondition :: Table -> FilePath -> Int -> String -> Bool
ifCondition tbl path line rest =
  evalExpr tbl (expandLine tbl path line (resolveDefined tbl (stripComments rest)) False) /= 0

-- | Replace each @defined X@ and @defined(X)@ with @1@ or @0@ before expansion
-- reaches it, so a macro that is defined as something numeric does not get
-- substituted into its own @defined@ test.
resolveDefined :: Table -> String -> String
resolveDefined tbl = go
  where
    go [] = []
    go (q : r)
      | q == '"' || q == '\'' = let (lit, r') = scanLit q r in q : lit ++ go r'
    go s@(c : r)
      | isIdentStart c =
          let (i, r') = span isIdentCont s
           in if i == "defined" then defOp r' else i ++ go r'
      | otherwise = c : go r

    defOp r0 =
      let (paren, r1) = case dropWhile isSpaceTab r0 of
            ('(' : r) -> (True, dropWhile isSpaceTab r)
            r -> (False, r)
          (mid, r2) = readIdent r1
          r3
            | paren = case dropWhile isSpaceTab r2 of
                (')' : r) -> r
                r -> r
            | otherwise = r2
          found = case mid of
            Just i -> maybe False (const True) (macroFind tbl i)
            Nothing -> False
       in (if found then "1" else "0") ++ go r3

directive :: FilePath -> String -> Int -> [ByteString] -> PPM [ByteString]
directive path raw line acc = do
  let afterHash = dropWhile isSpaceTab (drop 1 (dropWhile isSpaceTab raw))
      (dir, s2) = readIdent afterHash
      rest = dropWhile isSpaceTab s2
  case dir of
    Just "endif" -> do
      stk <- gets stIfStack
      if null stk
        then ppError path line "#endif without #if"
        else do
          modify $ \s -> s {stIfStack = drop 1 (stIfStack s)}
          recompute
      return (nl acc)
    Just "else" -> do
      stk <- gets stIfStack
      case stk of
        [] -> ppError path line "#else without #if"
        (t : ts) -> do
          modify $ \s -> s {stIfStack = flipTop t : ts}
          recompute
      return (nl acc)
    Just "elif" -> do
      stk <- gets stIfStack
      tbl <- gets stMacros
      case stk of
        [] -> ppError path line "#elif without #if"
        (t : ts) -> do
          let t'
                | t == 1 = 2
                | t == 0 = if ifCondition tbl path line rest then 1 else 0
                | otherwise = t
          modify $ \s -> s {stIfStack = t' : ts}
          recompute
      return (nl acc)
    Just d | d == "if" || d == "ifdef" || d == "ifndef" -> do
      tbl <- gets stMacros
      dis <- gets stDisabled
      let take_
            | d == "if" = ifCondition tbl path line rest
            | otherwise =
                let def = case fst (readIdent rest) of
                      Just n -> maybe False (const True) (macroFind tbl n)
                      Nothing -> False
                 in if d == "ifdef" then def else not def
          -- a conditional nested in dead code can never become live
          top
            | dis = 2
            | take_ = 1
            | otherwise = 0
      modify $ \s -> s {stIfStack = top : stIfStack s}
      recompute
      return (nl acc)
    _ -> do
      dis <- gets stDisabled
      if dis
        then return (nl acc)
        else case dir of
          Just "define" -> doDefine path line rest >> return (nl acc)
          Just "undef" -> do
            case fst (readIdent rest) of
              Just n -> modify $ \s -> s {stMacros = macroUndef n (stMacros s)}
              Nothing -> return ()
            return (nl acc)
          Just "include" -> doInclude path line rest acc
          Just "error" -> ppError path line ("#error " ++ rest) >> return (nl acc)
          Just "line" -> doLine path line rest acc
          _ -> return (nl acc)
  where
    nl a = nlB : a
    flipTop t
      | t == 1 = 2
      | t == 0 = 1
      | otherwise = t

doDefine :: FilePath -> Int -> String -> PPM ()
doDefine path line rest0 = case readIdent rest of
  (Nothing, _) -> ppError path line "#define missing name"
  (Just name, afterName) -> do
    let (isFn, variadic, params, afterParams) = case afterName of
          ('(' : r) ->
            let (ps, va, r') = parseParams (dropWhile isSpaceTab r)
             in (True, va, ps, r')
          _ -> (False, False, [], afterName)
        body = trimEnd (dropWhile isSpaceTab afterParams)
        m =
          Macro
            { mName = name
            , mFunc = isFn
            , mVariadic = variadic
            , mParams = params
            , mBody = body
            , mPredef = False
            }
    modify $ \s -> s {stMacros = macroDefine m (stMacros s)}
  where
    -- before the name, so a comment between the parameters goes too
    rest = stripComments rest0

    trimEnd = reverse . dropWhile isSpaceTab . reverse

    parseParams s0
      | (')' : _) <- s0 = ([], False, closeParen s0)
      | otherwise = loop s0
      where
        loop s =
          let s1 = dropWhile isSpaceTab s
           in if "..." `isPrefixOf` s1
                then ([], True, closeParen (drop 3 s1))
                else
                  let (mn, s2) = readIdent s1
                      s3 = dropWhile isSpaceTab s2
                   in case s3 of
                        (',' : s4) ->
                          let (ps, va, s5) = loop s4
                           in (maybe ps (: ps) mn, va, s5)
                        _ -> (maybe [] (: []) mn, False, closeParen s3)

    closeParen s = case dropWhile (/= ')') s of
      (')' : r) -> r
      r -> r

doInclude :: FilePath -> Int -> String -> [ByteString] -> PPM [ByteString]
doInclude path line rest acc = do
  tbl <- gets stMacros
  let target = case rest of
        ('"' : _) -> parseName rest
        ('<' : _) -> parseName rest
        _ -> parseName (dropWhile isSpaceTab (expandLine tbl path line rest False))
  case target of
    Nothing -> ppError path line "bad #include" >> return (nl acc)
    Just (angled, name) -> do
      paths <- gets stPaths
      cache <- gets stCache
      found <- liftIO (cachedResolve cache paths path name angled)
      case found of
        Nothing -> do
          ppError path line ("cannot find include \"" ++ name ++ "\"")
          return (nl acc)
        Just fp -> do
          msrc <- liftIO (cachedRead cache fp)
          case msrc of
            Nothing -> do
              ppError path line ("cannot read \"" ++ fp ++ "\"")
              return (nl acc)
            Just src -> do
              incOut <- ppBuffer fp src
              let acc1 = incOut : acc
                  acc2 = if lastCharIsNL acc1 then acc1 else nlB : acc1
              return (lineMarker (line + 1) path : acc2)
  where
    nl a = nlB : a
    parseName ('"' : r) = Just (False, takeWhile (/= '"') r)
    parseName ('<' : r) = Just (True, takeWhile (/= '>') r)
    parseName _ = Nothing

lastCharIsNL :: [ByteString] -> Bool
lastCharIsNL cs = case filter (not . BS.null) cs of
  (c : _) -> BS.last c == '\n'
  [] -> False

doLine :: FilePath -> Int -> String -> [ByteString] -> PPM [ByteString]
doLine path line rest acc = do
  tbl <- gets stMacros
  let e0 = dropWhile isSpaceTab (expandLine tbl path line rest False)
      (newLine, e1) = readDec e0
      e2 = dropWhile isSpaceTab e1
      newFile = case e2 of
        ('"' : r) -> takeWhile (/= '"') r
        _ -> path
  if newLine > 0
    then return (lineMarker newLine newFile : acc)
    else return (nlB : acc)

-- ---- predefined macros ----

predefs :: Table
predefs = foldl' (flip macroDefine) emptyTable (objs ++ fnNoops ++ objNoops ++ [popcount])
  where
    objs =
      [ objMacro True n v
      | (n, v) <-
          [ ("__C99METTLE__", "1")
          , ("__STDC__", "1")
          , ("__STDC_VERSION__", "199901L")
          , ("__x86_64__", "1")
          , ("_WIN32", "1")
          , ("_WIN64", "1")
          , ("__WIN32__", "1")
          ]
      ]

    -- Win64 has one calling convention and attributes do not reach this
    -- backend, so the MSVC/GCC decorations expand to nothing.
    fnNoops =
      [ (objMacro True n "") {mFunc = True, mParams = ["x"]}
      | n <- ["__declspec", "__attribute__", "__pragma"]
      ]

    objNoops =
      [ objMacro True n ""
      | n <-
          [ "__stdcall"
          , "__cdecl"
          , "__fastcall"
          , "__forceinline"
          , "__inline"
          , "__restrict"
          , "__unaligned"
          ]
      ]

    -- SWAR popcount as a pure expression; the argument is evaluated repeatedly.
    popcount = (objMacro True "__builtin_popcount" body) {mFunc = True, mParams = ["x"]}
      where
        a = "(((unsigned)(x)) - ((((unsigned)(x)) >> 1) & 0x55555555u))"
        b = "(((" ++ a ++ ") & 0x33333333u) + (((" ++ a ++ ") >> 2) & 0x33333333u))"
        body =
          "((int)((((((" ++ b ++ ") + ((" ++ b ++ ") >> 4)) & 0x0f0f0f0fu)"
            ++ " * 0x01010101u) >> 24) & 0xff))"

-- ---- entry point ----

-- | Preprocess the file at the given path. Returns the expanded text and any
-- diagnostics. Reads @#include@'d files from disk, hence IO.
preprocess :: PPOptions -> FilePath -> IO (ByteString, [Message])
preprocess opts path = do
  msrc <- cachedRead (ppCache opts) path
  case msrc of
    Nothing -> return (BS.empty, [diag Error (SrcLoc path 1 1) "cannot read file"])
    Just src -> do
      let st0 =
            St
              { stMacros = foldl' (flip macroDefine) predefs (map cmdline (ppDefines opts))
              , stIfStack = []
              , stDisabled = False
              , stIncludeStack = []
              , stMsgs = []
              , stPaths = ppIncludeDirs opts ++ ["include"]
              , stCache = ppCache opts
              }
      (out, st) <- runStateT (ppBuffer path src) st0
      return (out, reverse (stMsgs st))
  where
    -- `-DFOO` with no `=value` means 1, as it does for every other C compiler.
    cmdline (n, v) = objMacro False n (if null v then "1" else v)
