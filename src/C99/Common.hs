-- | Source locations and diagnostics (src/common.h).
--
-- The C frontend threads an arena and a mutable Diag counter through every
-- pass. Here the arena is the GC's problem, and diagnostics are values: a pass
-- returns the messages it produced alongside its result.
module C99.Common
  ( SrcLoc (..)
  , noLoc
  , Severity (..)
  , Message (..)
  , formatMessage
  , hasErrors
  , errorCount
  ) where

-- | A point in a source file. @file@ tracks @#line@ / @# n "file"@ markers, so
-- it names the original header, not the preprocessed intermediate.
data SrcLoc = SrcLoc
  { locFile :: FilePath
  , locLine :: !Int
  , locCol :: !Int
  }
  deriving (Eq, Show)

noLoc :: SrcLoc
noLoc = SrcLoc "<none>" 0 0

data Severity = Error | Warning
  deriving (Eq, Show)

data Message = Message
  { msgSeverity :: !Severity
  , msgLoc :: SrcLoc
  , msgText :: String
  }
  deriving (Eq, Show)

-- | @file:line:col: error: text@ — the shape diag_error prints, so existing
-- test expectations and editors keep working.
formatMessage :: Message -> String
formatMessage (Message sev (SrcLoc file line col) text) =
  file
    ++ ":"
    ++ show line
    ++ ":"
    ++ show col
    ++ ": "
    ++ label
    ++ ": "
    ++ text
  where
    label = case sev of
      Error -> "error"
      Warning -> "warning"

hasErrors :: [Message] -> Bool
hasErrors = any ((== Error) . msgSeverity)

errorCount :: [Message] -> Int
errorCount = length . filter ((== Error) . msgSeverity)
