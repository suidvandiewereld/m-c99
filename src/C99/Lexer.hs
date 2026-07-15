-- | The lexer (src/lexer.c).
--
-- Consumes preprocessed source and yields a token list. It still understands
-- @# n "file"@ line markers, because the preprocessor emits them and the
-- locations in every later diagnostic depend on tracking them.
module C99.Lexer
  ( tokenize
  ) where

import Control.Monad (replicateM_, unless, void, when)
import Control.Monad.State.Strict
import Data.Char (chr, digitToInt, isDigit, isHexDigit, isOctDigit, ord)

import C99.Common (Message (..), Severity (..), SrcLoc (..))
import C99.Token

data LexState = LexState
  { lsRest :: String
  , lsFile :: FilePath
  , lsLine :: !Int
  , lsCol :: !Int
  , lsMsgs :: [Message] -- reversed
  }

type Lex a = State LexState a

-- | Tokenize @src@, which is attributed to @path@ until a line marker says
-- otherwise. The token list always ends with TkEof.
tokenize :: FilePath -> String -> ([Token], [Message])
tokenize path src =
  let (toks, st) = runState loop (LexState src path 1 1 [])
   in (toks, reverse (lsMsgs st))
  where
    loop = do
      t <- next
      case tokKind t of
        TkEof -> pure [t]
        _ -> (t :) <$> loop

-- ---- primitives ----

peekC :: Lex Char
peekC = gets (headOr '\0' . lsRest)

peek2C :: Lex Char
peek2C = gets (headOr '\0' . drop 1 . lsRest)

headOr :: Char -> String -> Char
headOr d [] = d
headOr _ (c : _) = c

atEnd :: Lex Bool
atEnd = gets (null . lsRest)

advance :: Lex Char
advance = do
  st <- get
  case lsRest st of
    [] -> pure '\0'
    (c : cs) -> do
      put $
        if c == '\n'
          then st {lsRest = cs, lsLine = lsLine st + 1, lsCol = 1}
          else st {lsRest = cs, lsCol = lsCol st + 1}
      pure c

here :: Lex SrcLoc
here = gets $ \st -> SrcLoc (lsFile st) (lsLine st) (lsCol st)

err :: SrcLoc -> String -> Lex ()
err loc text = modify' $ \st ->
  st {lsMsgs = Message Error loc text : lsMsgs st}

-- | Consume while the predicate holds, returning what was consumed.
takeWhileL :: (Char -> Bool) -> Lex String
takeWhileL p = do
  c <- peekC
  done <- atEnd
  if not done && p c
    then (c :) <$> (advance >> takeWhileL p)
    else pure ""

skipWhile :: (Char -> Bool) -> Lex ()
skipWhile p = () <$ takeWhileL p

-- ---- whitespace, comments, line markers ----

skipTrivia :: Lex ()
skipTrivia = do
  done <- atEnd
  if done
    then pure ()
    else do
      c <- peekC
      c2 <- peek2C
      case () of
        _
          | c `elem` " \t\r\n\v\f" -> advance >> skipTrivia
          | c == '/' && c2 == '/' -> skipWhile (/= '\n') >> skipTrivia
          | c == '/' && c2 == '*' -> blockComment >> skipTrivia
          | c == '#' -> directive >> skipTrivia
          | otherwise -> pure ()

blockComment :: Lex ()
blockComment = do
  start <- here
  _ <- advance -- /
  _ <- advance -- *
  go start
  where
    go start = do
      done <- atEnd
      if done
        then err start "unterminated block comment"
        else do
          c <- peekC
          c2 <- peek2C
          if c == '*' && c2 == '/'
            then advance >> advance >> pure ()
            else advance >> go start

-- | @# 42 "foo.h"@ (and @#line 42 "foo.h"@) resync the reported location.
-- Any other residual directive is skipped to end of line.
directive :: Lex ()
directive = do
  _ <- advance -- #
  skipWhile (`elem` " \t")
  rest <- gets lsRest
  when (take 4 rest == "line") $ do
    replicateM_ 4 advance
    skipWhile (`elem` " \t")
  c <- peekC
  if isDigit c
    then do
      digits <- takeWhileL isDigit
      skipWhile (`elem` " \t")
      q <- peekC
      fname <-
        if q == '"'
          then do
            _ <- advance
            name <- takeWhileL (\x -> x /= '"' && x /= '\n')
            q' <- peekC
            when (q' == '"') (void advance)
            pure (Just name)
          else pure Nothing
      skipWhile (/= '\n')
      done <- atEnd
      unless done (void advance) -- the newline; the next line is `digits`
      modify' $ \st ->
        st
          { lsLine = read digits
          , lsCol = 1
          , lsFile = maybe (lsFile st) id fname
          }
    else skipWhile (/= '\n')

-- ---- literals ----

isIdentStart :: Char -> Bool
isIdentStart c = c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isIdentCont :: Char -> Bool
isIdentCont c = isIdentStart c || isDigit c

-- | One escape sequence, after the backslash has been consumed.
escape :: Lex Char
escape = do
  loc <- here
  c <- advance
  case c of
    '\'' -> pure '\''
    '"' -> pure '"'
    '?' -> pure '?'
    '\\' -> pure '\\'
    'a' -> pure '\a'
    'b' -> pure '\b'
    'f' -> pure '\f'
    'n' -> pure '\n'
    'r' -> pure '\r'
    't' -> pure '\t'
    'v' -> pure '\v'
    _
      | isOctDigit c -> do
          -- up to two more octal digits
          more <- octalTail 2
          pure (chr (foldl (\a d -> a * 8 + d) (digitToInt c) more `mod` 256))
      | c == 'x' -> do
          ds <- takeWhileL isHexDigit
          if null ds
            then do
              err loc "\\x used with no following hex digits"
              pure '\0'
            else
              pure (chr (foldl (\a d -> a * 16 + digitToInt d) 0 ds `mod` 256))
      | otherwise -> do
          err loc ("unknown escape sequence '\\" ++ [c] ++ "'")
          pure c
  where
    octalTail :: Int -> Lex [Int]
    octalTail 0 = pure []
    octalTail n = do
      d <- peekC
      if isOctDigit d
        then advance >> ((digitToInt d :) <$> octalTail (n - 1))
        else pure []

-- | A string literal, including C's adjacent-literal concatenation: the
-- lexer, not the parser, splices @"a" "b"@ into one token.
lexString :: Lex Token
lexString = do
  start <- here
  body <- one start
  more <- concatAdjacent
  let s = body ++ more
  pure
    emptyToken
      { tokKind = TkString
      , tokLoc = start
      , tokText = s
      , tokIVal = fromIntegral (length s) -- length excluding NUL
      }
  where
    one start = do
      _ <- advance -- opening quote
      s <- chars
      c <- peekC
      if c == '"'
        then advance >> pure s
        else err start "unterminated string literal" >> pure s

    chars = do
      done <- atEnd
      c <- peekC
      if done || c == '"' || c == '\n'
        then pure ""
        else
          if c == '\\'
            then do
              _ <- advance
              ch <- escape
              (ch :) <$> chars
            else do
              ch <- advance
              (ch :) <$> chars

    concatAdjacent = do
      skipTrivia
      c <- peekC
      if c /= '"'
        then pure ""
        else do
          start <- here
          s <- one start
          (s ++) <$> concatAdjacent

lexChar :: Lex Token
lexChar = do
  start <- here
  _ <- advance -- opening quote
  c <- peekC
  ch <-
    if c == '\\'
      then advance >> escape
      else do
        done <- atEnd
        if done || c == '\''
          then pure '\0'
          else advance
  q <- peekC
  if q == '\''
    then void advance
    else err start "unterminated character literal"
  -- C's plain char is signed here, but the value is taken as an unsigned byte,
  -- matching lex_char's (unsigned char) cast.
  pure
    emptyToken
      { tokKind = TkChar
      , tokLoc = start
      , tokIVal = fromIntegral (ord ch `mod` 256)
      }

lexNumber :: Lex Token
lexNumber = do
  start <- here
  c <- peekC
  c2 <- peek2C
  (digits, base, sawFloat) <-
    if c == '0' && (c2 == 'x' || c2 == 'X')
      then do
        _ <- advance
        _ <- advance
        ds <- takeWhileL isHexDigit
        pure (ds, 16, False)
      else
        if c == '0' && isOctDigit c2
          then do
            _ <- advance
            ds <- takeWhileL isOctDigit
            pure (ds, 8, False)
          else do
            intPart <- takeWhileL isDigit
            dot <- peekC
            (frac, isF1) <-
              if dot == '.'
                then do
                  -- C99 6.4.4.2: "1." and "1.f" are valid
                  _ <- advance
                  ds <- takeWhileL isDigit
                  pure ("." ++ ds, True)
                else pure ("", False)
            e <- peekC
            (expo, isF2) <-
              if e == 'e' || e == 'E'
                then do
                  _ <- advance
                  sgn <- peekC
                  sgnS <-
                    if sgn == '+' || sgn == '-'
                      then (: []) <$> advance
                      else pure ""
                  ds <- takeWhileL isDigit
                  pure ("e" ++ sgnS ++ ds, True)
                else pure ("", False)
            pure (intPart ++ frac ++ expo, 10, isF1 || isF2)
  (uns, lng, llng, fsuf) <- suffixes
  let isFloat = sawFloat || fsuf
  pure $
    if isFloat
      then
        emptyToken
          { tokKind = TkFloat
          , tokLoc = start
          , tokFVal = readDouble digits
          , tokFloatSuf = fsuf
          , tokUnsigned = uns
          , tokLong = lng
          , tokLongLong = llng
          }
      else
        emptyToken
          { tokKind = TkInt
          , tokLoc = start
          , tokIVal = readInteger base digits
          , tokUnsigned = uns
          , tokLong = lng
          , tokLongLong = llng
          }
  where
    suffixes = go (False, False, False, False)
      where
        go acc@(u, l, ll, f) = do
          c <- peekC
          case c of
            _
              | c == 'u' || c == 'U' -> advance >> go (True, l, ll, f)
              | c == 'l' || c == 'L' -> do
                  _ <- advance
                  c2 <- peekC
                  if c2 == 'l' || c2 == 'L'
                    then advance >> go (u, l, True, f)
                    else go (u, True, ll, f)
              | c == 'f' || c == 'F' -> advance >> go (u, l, ll, True)
              | otherwise -> pure acc

readInteger :: Int -> String -> Integer
readInteger base = foldl step 0
  where
    step acc d = acc * fromIntegral base + fromIntegral (digitToInt d)

-- | strtod, minus the parts C's grammar already excluded. Haskell's `read`
-- rejects the forms C accepts ("1.", ".5", "1e5"), so normalize first.
readDouble :: String -> Double
readDouble s = case reads (normalize s) of
  [(d, _)] -> d
  _ -> 0
  where
    normalize t =
      let t1 = if take 1 t == "." then '0' : t else t
          (mant, rest) = span (\c -> c /= 'e' && c /= 'E') t1
          mant' = if last' mant == '.' then mant ++ "0" else mant
          mant'' = if '.' `elem` mant' then mant' else mant' ++ ".0"
       in mant'' ++ expo rest
    expo "" = ""
    expo (_ : sgn) = case sgn of
      ('+' : ds) -> "e" ++ ds
      ('-' : ds) -> "e-" ++ ds
      ds -> "e" ++ ds
    last' [] = '\0'
    last' xs = last xs

-- ---- the main dispatch ----

next :: Lex Token
next = do
  skipTrivia
  done <- atEnd
  start <- here
  if done
    then pure emptyToken {tokKind = TkEof, tokLoc = start}
    else do
      c <- peekC
      c2 <- peek2C
      case () of
        _
          | isIdentStart c -> do
              s <- takeWhileL isIdentCont
              pure
                emptyToken
                  { tokKind = keywordKind s
                  , tokLoc = start
                  , tokText = s
                  }
          | isDigit c -> lexNumber
          | c == '.' && isDigit c2 -> lexNumber
          | c == '"' -> lexString
          | c == '\'' -> lexChar
          | otherwise -> punct start

-- | Punctuators, longest match first.
punct :: SrcLoc -> Lex Token
punct start = do
  c <- advance
  c2 <- peekC
  c3 <- peek2C
  let tok k = pure emptyToken {tokKind = k, tokLoc = start}
      tok2 k = advance >> tok k
      tok3 k = advance >> advance >> tok k
  case c of
    '(' -> tok TkLParen
    ')' -> tok TkRParen
    '{' -> tok TkLBrace
    '}' -> tok TkRBrace
    '[' -> tok TkLBracket
    ']' -> tok TkRBracket
    ';' -> tok TkSemi
    ',' -> tok TkComma
    '?' -> tok TkQuestion
    '~' -> tok TkTilde
    ':' -> tok TkColon
    '.'
      | c2 == '.' && c3 == '.' -> tok3 TkEllipsis
      | otherwise -> tok TkDot
    '+'
      | c2 == '+' -> tok2 TkInc
      | c2 == '=' -> tok2 TkAddAssign
      | otherwise -> tok TkPlus
    '-'
      | c2 == '-' -> tok2 TkDec
      | c2 == '=' -> tok2 TkSubAssign
      | c2 == '>' -> tok2 TkArrow
      | otherwise -> tok TkMinus
    '*'
      | c2 == '=' -> tok2 TkMulAssign
      | otherwise -> tok TkStar
    '/'
      | c2 == '=' -> tok2 TkDivAssign
      | otherwise -> tok TkSlash
    '%'
      | c2 == '=' -> tok2 TkModAssign
      | otherwise -> tok TkPercent
    '&'
      | c2 == '&' -> tok2 TkAndAnd
      | c2 == '=' -> tok2 TkAndAssign
      | otherwise -> tok TkAmp
    '|'
      | c2 == '|' -> tok2 TkOrOr
      | c2 == '=' -> tok2 TkOrAssign
      | otherwise -> tok TkPipe
    '^'
      | c2 == '=' -> tok2 TkXorAssign
      | otherwise -> tok TkCaret
    '!'
      | c2 == '=' -> tok2 TkNe
      | otherwise -> tok TkBang
    '='
      | c2 == '=' -> tok2 TkEq
      | otherwise -> tok TkAssign
    '<'
      | c2 == '<' && c3 == '=' -> tok3 TkLShiftAssign
      | c2 == '<' -> tok2 TkLShift
      | c2 == '=' -> tok2 TkLe
      | otherwise -> tok TkLt
    '>'
      | c2 == '>' && c3 == '=' -> tok3 TkRShiftAssign
      | c2 == '>' -> tok2 TkRShift
      | c2 == '=' -> tok2 TkGe
      | otherwise -> tok TkGt
    _ -> do
      err start ("unexpected character '" ++ [c] ++ "'")
      tok TkEof
