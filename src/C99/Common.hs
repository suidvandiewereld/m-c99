-- | Source locations and diagnostics (src/common.h).
--
-- The C frontend threads an arena and a mutable Diag counter through every
-- pass. Here the arena is the GC's problem, and diagnostics are values: a pass
-- returns the messages it produced alongside its result.
--
-- A 'Message' carries enough to render a rustc-shaped report: a span to
-- underline, an error code, an inline label on the caret run, a help line, and
-- attached notes that point at a second place in the source. Only the location
-- and text are required, so a pass can start plain and get richer one call site
-- at a time. 'C99.Diag' does the rendering.
module C99.Common
  ( SrcLoc (..)
  , noLoc
  , Severity (..)
  , Message (..)
  , diag
  , withCode
  , withLen
  , withLabel
  , withHelp
  , withNotes
  , withSnap
  , note
  , WarnGroup (..)
  , warnGroupName
  , warnGroupBlurb
  , allWarnGroups
  , formatMessage
  , hasErrors
  , errorCount
  , warningCount
  ) where

-- | A point in a source file. @file@ tracks @#line@ / @# n "file"@ markers, so
-- it names the original header, not the preprocessed intermediate.
data SrcLoc = SrcLoc
  { locFile :: FilePath
  , locLine :: !Int
  , locCol :: !Int
  }
  deriving (Eq, Ord, Show)

noLoc :: SrcLoc
noLoc = SrcLoc "<none>" 0 0

data Severity = Error | Warning | Note
  deriving (Eq, Ord, Show)

-- | Warnings are grouped so @-Wno-unused@ and friends can switch them off. A
-- diagnostic with no group cannot be disabled.
--
-- Only groups that actually fire belong here. A flag that silences nothing is
-- worse than a missing flag: it tells someone the compiler checks something it
-- does not. Add the constructor in the same change as the check.
data WarnGroup
  = WUnused
  | WUnreachable
  | WMissingReturn
  deriving (Eq, Ord, Show, Enum, Bounded)

warnGroupName :: WarnGroup -> String
warnGroupName g = case g of
  WUnused -> "unused"
  WUnreachable -> "unreachable-code"
  WMissingReturn -> "missing-return"

-- | One line each for @--help-warnings@.
warnGroupBlurb :: WarnGroup -> String
warnGroupBlurb g = case g of
  WUnused -> "a block-scope variable nothing reads"
  WUnreachable -> "a statement control can never arrive at"
  WMissingReturn -> "a value-returning function that can run off its end"

allWarnGroups :: [WarnGroup]
allWarnGroups = [minBound .. maxBound]

data Message = Message
  { msgSeverity :: !Severity
  , msgLoc :: SrcLoc
  , msgText :: String
  , -- | Characters to underline from 'msgLoc'. 0 means "point at one column",
    -- which is all a pass can say when it has no token length to hand.
    msgLen :: !Int
  , -- | @E0001@ and friends. Stable, so tests and docs can name them.
    msgCode :: Maybe String
  , -- | Written after the caret run, e.g. @expected 'int', found 'char *'@.
    msgLabel :: Maybe String
  , msgHelp :: Maybe String
  , -- | The warning group that can switch this off. Errors leave it 'Nothing'.
    msgGroup :: Maybe WarnGroup
  , -- | An identifier the caret should snap to. A declaration's location
    -- starts at its type ("int x"), so underlining from there points at the
    -- wrong word; the renderer looks for this name on the line instead.
    msgSnap :: Maybe String
  , -- | Secondary spans, each rendered as its own frame under the primary.
    msgNotes :: [Message]
  }
  deriving (Eq, Show)

-- | The one required shape. Everything else is a @with*@ on top, so a pass that
-- only knows a location and a sentence still type-checks.
diag :: Severity -> SrcLoc -> String -> Message
diag sev loc text =
  Message
    { msgSeverity = sev
    , msgLoc = loc
    , msgText = text
    , msgLen = 0
    , msgCode = Nothing
    , msgLabel = Nothing
    , msgHelp = Nothing
    , msgGroup = Nothing
    , msgSnap = Nothing
    , msgNotes = []
    }

note :: SrcLoc -> String -> Message
note = diag Note

withCode :: String -> Message -> Message
withCode c m = m {msgCode = Just c}

withLen :: Int -> Message -> Message
withLen n m = m {msgLen = max 0 n}

withLabel :: String -> Message -> Message
withLabel l m = m {msgLabel = Just l}

withHelp :: String -> Message -> Message
withHelp h m = m {msgHelp = Just h}

withSnap :: String -> Message -> Message
withSnap n m = m {msgSnap = Just n}

withNotes :: [Message] -> Message -> Message
withNotes ns m = m {msgNotes = msgNotes m ++ ns}

-- | @file:line:col: error: text@, the one-line form. 'C99.Diag' renders the
-- full report; this stays for callers that want a bare line.
formatMessage :: Message -> String
formatMessage m =
  locFile loc
    ++ ":"
    ++ show (locLine loc)
    ++ ":"
    ++ show (locCol loc)
    ++ ": "
    ++ label
    ++ ": "
    ++ msgText m
  where
    loc = msgLoc m
    label = case msgSeverity m of
      Error -> "error"
      Warning -> "warning"
      Note -> "note"

hasErrors :: [Message] -> Bool
hasErrors = any ((== Error) . msgSeverity)

errorCount :: [Message] -> Int
errorCount = length . filter ((== Error) . msgSeverity)

warningCount :: [Message] -> Int
warningCount = length . filter ((== Warning) . msgSeverity)
