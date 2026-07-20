-- | The c99mtlc driver (src/main.c).
--
-- preprocess -> lex -> parse -> mangle statics -> merge -> check -> lower ->
-- optimize -> emit.
module Main (main) where

import Control.Monad (forM, unless, when)
import qualified Data.ByteString.Char8 as BS
import Data.List (isPrefixOf)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hIsTerminalDevice, hPutStrLn, stderr)

import C99.Common
import C99.CType (TypeContext, newTypeContext)
import C99.Diag
import C99.Explain (explain, explainIndex)
import C99.Ast (Program)
import C99.Lexer (tokenize)
import C99.Lower (lowerProgram)
import C99.Parser (parseProgram)
import C99.Preprocess (PPOptions (..), newPPCache, preprocess)
import C99.Runtime (u128RuntimeSrc)
import C99.Sema (SemaResult (..), semaCheck)
import C99.StaticRename (mangleStatics)
import qualified Mtlc

data Options = Options
  { optOutput :: Maybe FilePath
  , optOptLevel :: !Int
  , optObjOnly :: !Bool
  , optPreprocessOnly :: !Bool
  , optEmitIrOnly :: !Bool
  , optStaticPrefix :: String
  , optIncludes :: [FilePath]
  , optInputs :: [FilePath]
  , -- | @auto@ unless @--color=always@/@never@ forced it.
    optColor :: Maybe Bool
  , optNoWarn :: [WarnGroup]
  , optWarnError :: !Bool
  , optMaxErrors :: !Int
  , optJson :: !Bool
  , optExplain :: Maybe String
  }

defaults :: Options
defaults =
  Options
    { optOutput = Nothing
    , optOptLevel = 1
    , optObjOnly = False
    , optPreprocessOnly = False
    , optEmitIrOnly = False
    , optStaticPrefix = "st"
    , optIncludes = []
    , optInputs = []
    , optColor = Nothing
    , optNoWarn = []
    , optWarnError = False
    , optMaxErrors = 100
    , optJson = False
    , optExplain = Nothing
    }

usage :: IO ()
usage = do
  v <- Mtlc.version
  mapM_
    (hPutStrLn stderr)
    [ "C99Mettle - C99 compiler (libmtlc backend)"
    , "Usage: c99mtlc [options] <file.c>..."
    , "Options:"
    , "  -o <path>     output executable (default: a.exe)"
    , "  -I <dir>      add include search path"
    , "  -E            preprocess only (write to stdout)"
    , "  -O0/-O1/-O    optimization off / on"
    , "  -c            emit object only (.obj)"
    , "  -S            emit object (same as -c; no asm text yet)"
    , "  --emit-ir     finish after lowering (smoke test; no output file)"
    , "  --static-prefix=<p>  prefix for file-scope static mangling"
    , "  -h, --help    this help"
    , ""
    , "Diagnostics:"
    , "  -Wno-<group>  silence a warning group (see --help-warnings)"
    , "  -Werror       treat warnings as errors"
    , "  --max-errors=N  stop after N errors (default 100, 0 = no limit)"
    , "  --error-format=json  one JSON object per diagnostic, for editors"
    , "  --color=auto|always|never  (auto honours NO_COLOR and CLICOLOR_FORCE)"
    , "  --explain <CODE>  what an error code means, and how to fix it"
    , "  --help-warnings   list the warning groups"
    , ""
    , "Backend: " ++ v
    ]

warningsHelp :: IO ()
warningsHelp =
  mapM_
    (hPutStrLn stderr)
    ( [ "Warning groups. Each is on by default and off with -Wno-<group>."
      , ""
      ]
        ++ map
          ( \g ->
              "  -Wno-"
                ++ warnGroupName g
                ++ replicate (max 1 (22 - length (warnGroupName g))) ' '
                ++ warnGroupBlurb g
          )
          allWarnGroups
        ++ [ ""
           , "  -Werror   turn every warning that is still on into an error"
           ]
    )

parseArgs :: [String] -> Options -> IO Options
parseArgs [] o = pure o {optInputs = reverse (optInputs o), optIncludes = reverse (optIncludes o)}
parseArgs (a : rest) o = case a of
  "-h" -> usage >> exitSuccess
  "--help" -> usage >> exitSuccess
  "--help-warnings" -> warningsHelp >> exitSuccess
  "--explain" -> case rest of
    (c : more) -> parseArgs more o {optExplain = Just c}
    [] -> fatal "missing argument for --explain"
  "-Werror" -> parseArgs rest o {optWarnError = True}
  "-w" -> parseArgs rest o {optNoWarn = allWarnGroups}
  "-o" -> case rest of
    (p : more) -> parseArgs more o {optOutput = Just p}
    [] -> fatal "missing argument for -o"
  "-I" -> case rest of
    (p : more) -> parseArgs more o {optIncludes = p : optIncludes o}
    [] -> fatal "missing argument for -I"
  "-E" -> parseArgs rest o {optPreprocessOnly = True}
  "-O0" -> parseArgs rest o {optOptLevel = 0}
  "-c" -> parseArgs rest o {optObjOnly = True}
  "-S" -> parseArgs rest o {optObjOnly = True}
  "--emit-ir" -> parseArgs rest o {optEmitIrOnly = True}
  _
    | a `elem` ["-O", "-O1", "-O2", "-O3"] -> parseArgs rest o {optOptLevel = 1}
    | "-I" `isPrefixOf` a, length a > 2 ->
        parseArgs rest o {optIncludes = drop 2 a : optIncludes o}
    | "--static-prefix=" `isPrefixOf` a ->
        parseArgs rest o {optStaticPrefix = drop 16 a}
    | "--explain=" `isPrefixOf` a -> parseArgs rest o {optExplain = Just (drop 10 a)}
    | "--max-errors=" `isPrefixOf` a -> case reads (drop 13 a) of
        [(n, "")] -> parseArgs rest o {optMaxErrors = if n <= 0 then maxBound else n}
        _ -> fatal ("--max-errors wants a number, got '" ++ drop 13 a ++ "'")
    | a == "--error-format=json" -> parseArgs rest o {optJson = True}
    | "--error-format=" `isPrefixOf` a ->
        fatal ("unknown --error-format '" ++ drop 15 a ++ "' (want: human, json)")
    | "--color=" `isPrefixOf` a -> case drop 8 a of
        "always" -> parseArgs rest o {optColor = Just True}
        "never" -> parseArgs rest o {optColor = Just False}
        "auto" -> parseArgs rest o {optColor = Nothing}
        v -> fatal ("unknown --color '" ++ v ++ "' (want: auto, always, never)")
    | "-Wno-" `isPrefixOf` a -> case lookupGroup (drop 5 a) of
        Just g -> parseArgs rest o {optNoWarn = g : optNoWarn o}
        Nothing -> fatal (unknownWarning (drop 5 a))
    | a == "-Wall" || a == "-Wextra" -> parseArgs rest o -- every group is already on
    | "-" `isPrefixOf` a -> fatal ("unknown option '" ++ a ++ "'")
    | otherwise -> parseArgs rest o {optInputs = a : optInputs o}

fatal :: String -> IO a
fatal msg = hPutStrLn stderr ("c99mtlc: " ++ msg) >> exitFailure

lookupGroup :: String -> Maybe WarnGroup
lookupGroup n = lookup n [(warnGroupName g, g) | g <- allWarnGroups]

-- | Getting a flag name slightly wrong should not mean reading the manual.
unknownWarning :: String -> String
unknownWarning n =
  "unknown warning group '"
    ++ n
    ++ "'"
    ++ case closestCandidate n (map warnGroupName allWarnGroups) of
      Just c -> "; did you mean '-Wno-" ++ c ++ "'?"
      Nothing -> "; see --help-warnings"

-- | @auto@: colour when stderr is a terminal, with the environment able to
-- override in both directions. CLICOLOR_FORCE wins over NO_COLOR, which is the
-- order everyone else settled on.
resolveColor :: Maybe Bool -> IO Bool
resolveColor (Just b) = pure b
resolveColor Nothing = do
  force <- lookupEnv "CLICOLOR_FORCE"
  no <- lookupEnv "NO_COLOR"
  term <- lookupEnv "TERM"
  tty <- hIsTerminalDevice stderr
  pure $ case () of
    _
      | Just v <- force, v /= "0" -> True
      | Just _ <- no -> False
      | Just "dumb" <- term -> False
      | otherwise -> tty

main :: IO ()
main = do
  args <- getArgs
  opts <- parseArgs args defaults

  case optExplain opts of
    Just c -> case explain c of
      Just body -> mapM_ putStrLn body >> exitSuccess
      Nothing
        | map toLowerAscii c `elem` ["list", "all"] ->
            mapM_ putStrLn explainIndex >> exitSuccess
        | otherwise -> do
            hPutStrLn stderr ("c99mtlc: no such error code '" ++ c ++ "'")
            mapM_ (hPutStrLn stderr) explainIndex
            exitFailure
    Nothing -> pure ()

  when (null (optInputs opts)) (usage >> exitFailure)

  color <- resolveColor (optColor opts)
  sink <-
    newDiagSink
      defaultDiagOptions
        { doColor = color
        , doSuppressed = optNoWarn opts
        , doWarnAsError = optWarnError opts
        , doMaxErrors = optMaxErrors opts
        , doJson = optJson opts
        }

  -- The builtin include dir (freestanding + CRT declarations) is searched
  -- last. One cache for the whole run: every TU includes the same headers,
  -- and this reads and resolves each of them once.
  cache <- newPPCache
  let ppopt =
        PPOptions
          { ppIncludeDirs = optIncludes opts ++ ["include"]
          , ppDefines = []
          , ppCache = Just cache
          }

  if optPreprocessOnly opts
    then do
      oks <- forM (optInputs opts) $ \path -> do
        (text, msgs) <- preprocess ppopt path
        sinkReport sink msgs
        if hasErrors msgs then pure False else BS.putStr text >> pure True
      unless (and oks) $ do
        finish sink (optInputs opts)
    else compile sink opts ppopt

toLowerAscii :: Char -> Char
toLowerAscii ch
  | ch >= 'A' && ch <= 'Z' = toEnum (fromEnum ch + 32)
  | otherwise = ch

-- | Parse one translation unit, threading the shared TypeContext so tags
-- accumulate across units.
--
-- Returns whatever it managed to parse even when it reported errors, so the
-- later passes can keep finding real problems in the parts that are intact.
-- Only a preprocessor failure yields nothing, because then there is no text to
-- lex at all.
parseTU
  :: DiagSink
  -> PPOptions
  -> TypeContext
  -> FilePath
  -> IO (Program, TypeContext, Bool, Bool)
parseTU sink ppopt tc path = do
  (text, ppMsgs) <- preprocess ppopt path
  sinkReport sink ppMsgs
  if hasErrors ppMsgs
    then pure ([], tc, False, True)
    else do
      let (toks, lexMsgs) = tokenize path text
          (prog, tc', sawI128, parseMsgs) = parseProgram tc toks
          msgs = lexMsgs ++ parseMsgs
      sinkReport sink msgs
      pure (prog, tc', sawI128, hasErrors msgs)

compile :: DiagSink -> Options -> PPOptions -> IO ()
compile sink opts ppopt = do
  let inputs = optInputs opts
      output = maybe (if optObjOnly opts then "a.obj" else "a.exe") id (optOutput opts)

  -- Parse every unit before giving up. Stopping at the first broken file hides
  -- the state of the rest, which turns one build into several: the whole point
  -- of a batch compiler is that one run tells you everything that is wrong.
  --
  -- Each unit's file-scope statics are mangled with its own index, which is
  -- what keeps them from colliding once the units are merged.
  (merged, tc1, sawI128, parseFailed) <-
    foldMTU (\i tc path -> do
      (prog, tc', saw, bad) <- parseTU sink ppopt tc path
      pure (mangleStatics (optStaticPrefix opts) i prog, tc', saw, bad))
      newTypeContext
      (zip [0 ..] inputs)

  -- Any unit that used __int128 needs the u128 helper runtime as one more unit.
  (merged', tc2) <-
    if not sawI128
      then pure (merged, tc1)
      else do
        let (toks, lexMsgs) = tokenize "<c99m-u128-runtime>" (BS.pack u128RuntimeSrc)
            (rprog, tc', _, pMsgs) = parseProgram tc1 toks
        sinkReport sink (lexMsgs ++ pMsgs)
        if hasErrors (lexMsgs ++ pMsgs)
          then fatal "failed to parse the __int128 runtime"
          else pure (merged ++ mangleStatics "stU" 0 rprog, tc')

  -- Check what parsed even when some of it did not. A syntax error and a type
  -- error in the same build should come out of the same run.
  let sr = semaCheck tc2 merged'
  sinkReport sink (semaAfterParseErrors parseFailed (srMsgs sr))
  -- Ask the sink, not the message list: -Werror turns a warning into an error
  -- during reporting, which the pass that produced it cannot know.
  semaFailed <- sinkFailed sink
  when semaFailed (finish sink inputs)

  (mmod, lowMsgs) <- lowerProgram sr
  sinkReport sink lowMsgs
  lowFailed <- sinkFailed sink
  case mmod of
    Nothing -> finish sink inputs
    Just m | lowFailed -> do
      Mtlc.moduleDestroy m
      finish sink inputs
    Just m -> do
      if optEmitIrOnly opts
        then do
          n <- Mtlc.moduleFunctionCount m
          putStrLn $
            "OK: lowered "
              ++ show (length inputs)
              ++ " file(s) ("
              ++ show n
              ++ " functions)"
          Mtlc.moduleDestroy m
        else do
          ctx <- Mtlc.contextCreate
          Mtlc.contextSetOptLevel ctx (optOptLevel opts)
          -- whole-program only holds when we link the whole executable
          Mtlc.contextSetWholeProgram ctx (not (optObjOnly opts))
          optOk <-
            if optOptLevel opts > 0
              then Mtlc.optimize ctx m
              else pure True
          unless optOk $ do
            hPutStrLn stderr "optimization failed"
            Mtlc.contextDestroy ctx
            Mtlc.moduleDestroy m
            exitFailure
          ok <-
            if optObjOnly opts
              then Mtlc.emitObject ctx m output
              else Mtlc.buildExecutable ctx m output
          Mtlc.contextDestroy ctx
          Mtlc.moduleDestroy m
          unless ok $ do
            hPutStrLn stderr "code generation / link failed"
            exitFailure

-- | Fold the translation units, accumulating the merged program, the shared
-- TypeContext, and whether any unit used __int128.
foldMTU
  :: (Int -> TypeContext -> FilePath -> IO (Program, TypeContext, Bool, Bool))
  -> TypeContext
  -> [(Int, FilePath)]
  -> IO (Program, TypeContext, Bool, Bool)
foldMTU f tc0 = go [] tc0 False False
  where
    -- units accumulate in reverse and concatenate once: appending each
    -- program to the merged tail re-copied everything already merged, which
    -- is quadratic in the number of declarations.
    go acc tc saw bad [] = pure (concat (reverse acc), tc, saw, bad)
    go acc tc saw bad ((i, path) : rest) = do
      (prog, tc', s, b) <- f i tc path
      go (prog : acc) tc' (saw || s) (bad || b) rest

-- | What to do with sema's findings when the parse did not fully succeed.
--
-- Errors are kept, which is the point: a syntax error and a type error in one
-- build should come out of one run. Some of them can be fallout, since a
-- declaration the parser could not read is one sema never saw, so later uses
-- of that name read as undeclared. Suppressing the whole class would undo the
-- reason for checking at all (it is the ordinary case, a missing semicolon in
-- one function and a real type error in another, that matters most). Instead
-- every undeclared-identifier error in this situation says on its own help
-- line that it may be fallout, so nobody has to know the rule to read the
-- output correctly.
--
-- Warnings are dropped. A warning is advice about code you intend to keep, and
-- on a file that does not parse it is as likely to be an artefact of the lost
-- tree as a real finding: an unused variable whose only use was in a statement
-- the parser dropped. Nobody acts on warnings in a build that failed anyway.
semaAfterParseErrors :: Bool -> [Message] -> [Message]
semaAfterParseErrors False ms = ms
semaAfterParseErrors True ms =
  map annotate (filter ((/= Warning) . msgSeverity) ms)
  where
    -- "did you mean" already explains the likely cause; leave those alone.
    annotate m
      | msgCode m == Just "E0102"
      , Nothing <- msgHelp m =
          withHelp
            "if this is declared above, a syntax error there may have hidden it; fix those first"
            m
      | otherwise = m

-- | Print the closing count and stop. Every failure path goes through here, so
-- the last line a build prints is always the same shape.
finish :: DiagSink -> [FilePath] -> IO a
finish sink inputs = do
  sinkSummary sink (case inputs of (p : _) -> p; [] -> "input")
  exitFailure
