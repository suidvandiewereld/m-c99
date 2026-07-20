-- | The c99mtlc driver (src/main.c).
--
-- preprocess -> lex -> parse -> mangle statics -> merge -> check -> lower ->
-- optimize -> emit.
module Main (main) where

import Control.Monad (forM, unless, when)
import Data.List (isPrefixOf)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)

import C99.Common
import C99.CType (TypeContext, newTypeContext)
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
    , "Backend: " ++ v
    ]

parseArgs :: [String] -> Options -> IO Options
parseArgs [] o = pure o {optInputs = reverse (optInputs o), optIncludes = reverse (optIncludes o)}
parseArgs (a : rest) o = case a of
  "-h" -> usage >> exitSuccess
  "--help" -> usage >> exitSuccess
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
    | "-" `isPrefixOf` a -> fatal ("unknown option '" ++ a ++ "'")
    | otherwise -> parseArgs rest o {optInputs = a : optInputs o}

fatal :: String -> IO a
fatal msg = hPutStrLn stderr ("c99mtlc: " ++ msg) >> exitFailure

main :: IO ()
main = do
  args <- getArgs
  opts <- parseArgs args defaults
  when (null (optInputs opts)) (usage >> exitFailure)

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
        report msgs
        if hasErrors msgs then pure False else putStr text >> pure True
      unless (and oks) (hPutStrLn stderr "preprocess failed" >> exitFailure)
    else compile opts ppopt

report :: [Message] -> IO ()
report = mapM_ (hPutStrLn stderr . formatMessage)

-- | Parse one translation unit, threading the shared TypeContext so tags
-- accumulate across units.
parseTU :: PPOptions -> TypeContext -> FilePath -> IO (Maybe (Program, TypeContext, Bool))
parseTU ppopt tc path = do
  (text, ppMsgs) <- preprocess ppopt path
  report ppMsgs
  if hasErrors ppMsgs
    then pure Nothing
    else do
      let (toks, lexMsgs) = tokenize path text
          (prog, tc', sawI128, parseMsgs) = parseProgram tc toks
          msgs = lexMsgs ++ parseMsgs
      report msgs
      if hasErrors msgs
        then do
          hPutStrLn stderr $
            show (errorCount msgs) ++ " error(s) generated while parsing " ++ path ++ "."
          pure Nothing
        else pure (Just (prog, tc', sawI128))

compile :: Options -> PPOptions -> IO ()
compile opts ppopt = do
  let inputs = optInputs opts
      output = maybe (if optObjOnly opts then "a.obj" else "a.exe") id (optOutput opts)

  -- Each unit's file-scope statics are mangled with its own index, which is
  -- what keeps them from colliding once the units are merged.
  (merged, tc1, sawI128) <-
    foldMTU (\i tc path -> do
      r <- parseTU ppopt tc path
      case r of
        Nothing -> exitFailure
        Just (prog, tc', saw) ->
          pure (mangleStatics (optStaticPrefix opts) i prog, tc', saw))
      newTypeContext
      (zip [0 ..] inputs)

  -- Any unit that used __int128 needs the u128 helper runtime as one more unit.
  (merged', tc2) <-
    if not sawI128
      then pure (merged, tc1)
      else do
        let (toks, lexMsgs) = tokenize "<c99m-u128-runtime>" u128RuntimeSrc
            (rprog, tc', _, pMsgs) = parseProgram tc1 toks
        report (lexMsgs ++ pMsgs)
        if hasErrors (lexMsgs ++ pMsgs)
          then fatal "failed to parse the __int128 runtime"
          else pure (merged ++ mangleStatics "stU" 0 rprog, tc')

  let sr = semaCheck tc2 merged'
  report (srMsgs sr)
  when (hasErrors (srMsgs sr)) $ do
    hPutStrLn stderr (show (errorCount (srMsgs sr)) ++ " error(s) generated.")
    exitFailure

  (mmod, lowMsgs) <- lowerProgram sr
  report lowMsgs
  case mmod of
    Nothing -> do
      hPutStrLn stderr ("lowering failed (" ++ show (errorCount lowMsgs) ++ " error(s)).")
      exitFailure
    Just m | hasErrors lowMsgs -> do
      Mtlc.moduleDestroy m
      hPutStrLn stderr ("lowering failed (" ++ show (errorCount lowMsgs) ++ " error(s)).")
      exitFailure
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
  :: (Int -> TypeContext -> FilePath -> IO (Program, TypeContext, Bool))
  -> TypeContext
  -> [(Int, FilePath)]
  -> IO (Program, TypeContext, Bool)
foldMTU f tc0 = go [] tc0 False
  where
    go acc tc saw [] = pure (acc, tc, saw)
    go acc tc saw ((i, path) : rest) = do
      (prog, tc', s) <- f i tc path
      go (acc ++ prog) tc' (saw || s) rest
