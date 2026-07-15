-- | The recursive-descent parser (src/parser.c).
--
-- The parser owns two tables that the grammar cannot do without: the typedef
-- names (an identifier is a type token exactly when it names a typedef) and the
-- enumerator values (array bounds and enumerator initializers are folded here,
-- at parse time, because a bound that folds is a fixed array and one that does
-- not is a VLA).
--
-- Struct, union and enum tags go into the 'TypeContext', which is threaded in
-- and out so that tags accumulate across translation units.
module C99.Parser
  ( parseProgram
  ) where

import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Control.Monad.State.Strict
import Data.Bits (complement, shiftL, shiftR, xor, (.&.), (.|.))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isNothing)

import C99.Ast
import C99.CType
import C99.Common (Message (..), Severity (..), SrcLoc)
import C99.Token

-- ---- state ----

data PState = PState
  { psToks :: [Token]
  , -- | Tokens consumed so far; only used to detect a stalled loop.
    psPos :: !Int
  , psTc :: TypeContext
  , psTypedefs :: M.Map String Type
  , psEnums :: M.Map String Integer
  , psMsgs :: [Message] -- reversed
  , psSawInt128 :: !Bool
  , -- | Whether the declarator being parsed wants the bound of a VLA reported,
    -- and the first such bound if one has been seen.
    psVlaOn :: !Bool
  , psVlaSize :: Maybe Expr
  , -- | Enumerators declared inside a function body. They are hoisted to the
    -- top level, which is the only place the AST can hold them.
    psPending :: [TopDecl]
  }

type P a = State PState a

-- | Parse a token list into a 'Program', returning the updated 'TypeContext',
-- whether any @__int128@ was seen (the driver then appends the u128 runtime),
-- and the diagnostics. Parse errors are recorded and recovered from; parsing
-- never stops early.
parseProgram :: TypeContext -> [Token] -> (Program, TypeContext, Bool, [Message])
parseProgram tc toks =
  let st0 =
        PState
          { psToks = toks
          , psPos = 0
          , psTc = tc
          , psTypedefs = M.empty
          , psEnums = M.empty
          , psMsgs = []
          , psSawInt128 = False
          , psVlaOn = False
          , psVlaSize = Nothing
          , psPending = []
          }
      (prog, st) = runState program st0
   in (prog, psTc st, psSawInt128 st, reverse (psMsgs st))

-- ---- token stream ----

cur :: P Token
cur = gets (headOr . psToks)
  where
    headOr (t : _) = t
    headOr [] = emptyToken

peekTok :: P Token
peekTok = gets (nth . psToks)
  where
    nth (_ : t : _) = t
    nth _ = emptyToken

curKind :: P TokenKind
curKind = tokKind <$> cur

peekKind :: P TokenKind
peekKind = tokKind <$> peekTok

curLoc :: P SrcLoc
curLoc = tokLoc <$> cur

-- | The trailing TkEof is never consumed, so the stream is inexhaustible.
advance :: P ()
advance = modify' step
  where
    step s = case psToks s of
      (_ : rest@(_ : _)) -> s {psToks = rest, psPos = psPos s + 1}
      _ -> s

check :: TokenKind -> P Bool
check k = (== k) <$> curKind

match :: TokenKind -> P Bool
match k = do
  ok <- check k
  when ok advance
  pure ok

expect :: TokenKind -> P ()
expect k = do
  t <- cur
  if tokKind t == k
    then advance
    else
      perr
        (tokLoc t)
        ("expected " ++ tokenKindName k ++ ", got " ++ tokenKindName (tokKind t))

perr :: SrcLoc -> String -> P ()
perr loc text = modify' $ \s -> s {psMsgs = Message Error loc text : psMsgs s}

-- ---- tables ----

registerTypedef :: String -> Type -> P ()
registerTypedef name ty =
  unless (null name) $
    modify' $
      \s -> s {psTypedefs = M.insert name ty (psTypedefs s)}

lookupTypedef :: String -> P (Maybe Type)
lookupTypedef name = gets (M.lookup name . psTypedefs)

registerEnum :: String -> Integer -> P ()
registerEnum name v = modify' $ \s -> s {psEnums = M.insert name v (psEnums s)}

lookupEnum :: String -> P (Maybe Integer)
lookupEnum name = gets (M.lookup name . psEnums)

declareTag :: TagKind -> Maybe String -> P TagId
declareTag k mname = do
  tc <- gets psTc
  let (tid, tc') = tagDeclare k mname tc
  modify' $ \s -> s {psTc = tc'}
  pure tid

setMembers :: TagId -> [Member] -> P ()
setMembers tid ms = modify' $ \s -> s {psTc = tagSetMembers tid ms (psTc s)}

pushPending :: [TopDecl] -> P ()
pushPending [] = pure ()
pushPending ds = modify' $ \s -> s {psPending = psPending s ++ ds}

takePending :: P [TopDecl]
takePending = do
  ds <- gets psPending
  modify' $ \s -> s {psPending = []}
  pure ds

-- | An identifier is a type token exactly when it names a typedef; without the
-- table the grammar is ambiguous.
isTypeToken :: P Bool
isTypeToken = do
  t <- cur
  case tokKind t of
    TkVoid -> pure True
    TkCharKw -> pure True
    TkShort -> pure True
    TkIntKw -> pure True
    TkLong -> pure True
    TkFloatKw -> pure True
    TkDouble -> pure True
    TkSigned -> pure True
    TkUnsigned -> pure True
    TkBool -> pure True
    TkStruct -> pure True
    TkUnion -> pure True
    TkEnum -> pure True
    TkConst -> pure True
    TkVolatile -> pure True
    TkRestrict -> pure True
    TkComplex -> pure True
    TkInt128 -> pure True
    TkIdent -> maybe False (const True) <$> lookupTypedef (tokText t)
    _ -> pure False

-- ---- expressions ----

parsePrimary :: P Expr
parsePrimary = do
  t <- cur
  let loc = tokLoc t
  case tokKind t of
    TkInt -> do
      advance
      let suf =
            IntSuffix
              (tokUnsigned t)
              (if tokLongLong t then 2 else if tokLong t then 1 else 0)
      pure (mkExpr loc (EInt (tokIVal t) suf))
    TkFloat -> advance >> pure (mkExpr loc (EFloat (tokFVal t) (tokFloatSuf t)))
    TkChar -> advance >> pure (mkExpr loc (EChar (tokIVal t)))
    TkString -> advance >> pure (mkExpr loc (EString (tokText t)))
    TkIdent -> advance >> pure (mkExpr loc (EIdent (tokText t) Nothing))
    TkLParen -> do
      advance
      isT <- isTypeToken
      if isT
        then do
          ty <- parseTypeName
          expect TkRParen
          brace <- check TkLBrace
          if brace
            then do
              i <- parseInitializer
              pure (mkExpr loc (ECompoundLit ty i))
            else do
              -- C99 6.5.4: the operand of a cast is a unary-expression, not a
              -- full conditional, so `(T)a < b` is `((T)a) < b`. A nested cast
              -- comes back through here via parsePrimary's '(' case.
              inner <- parseUnary
              pure (mkExpr loc (ECast ty inner))
        else do
          e <- parseExpr
          expect TkRParen
          pure e
    _ -> do
      perr loc "expected expression"
      advance
      pure (mkExpr loc (EInt 0 noSuffix))

parsePostfixExpr :: P Expr
parsePostfixExpr = parsePrimary >>= go
  where
    go e = do
      loc <- curLoc
      k <- curKind
      case k of
        TkLParen -> do
          advance
          if isVaArg e
            then do
              ap <- parseAssign
              expect TkComma
              ty <- parseTypeName
              expect TkRParen
              go (mkExpr loc (ECall e [ap] (Just ty)))
            else do
              args <- parseArgs
              expect TkRParen
              go (mkExpr loc (ECall e args Nothing))
        TkLBracket -> do
          advance
          idx <- parseExpr
          expect TkRBracket
          go (mkExpr loc (EIndex e idx))
        TkDot -> member loc False
        TkArrow -> member loc True
        TkInc -> advance >> go (mkExpr loc (EPostfix PostInc e))
        TkDec -> advance >> go (mkExpr loc (EPostfix PostDec e))
        _ -> pure e
      where
        member loc arrow = do
          advance
          idt <- cur
          expect TkIdent
          go (mkExpr loc (EMember e (tokText idt) arrow))

    isVaArg e = case exNode e of
      EIdent n Nothing -> n == "__builtin_va_arg"
      _ -> False

    parseArgs = do
      rp <- check TkRParen
      if rp then pure [] else argLoop
    argLoop = do
      a <- parseAssign
      more <- match TkComma
      if more then (a :) <$> argLoop else pure [a]

parseUnary :: P Expr
parseUnary = do
  loc <- curLoc
  k <- curKind
  case k of
    TkInc -> pre loc PreInc
    TkDec -> pre loc PreDec
    TkAmp -> pre loc AddrOf
    TkStar -> pre loc Deref
    TkPlus -> pre loc UPlus
    TkMinus -> pre loc Neg
    TkTilde -> pre loc BNot
    TkBang -> pre loc Not
    TkSizeof -> do
      advance
      paren <- check TkLParen
      if paren
        then do
          advance
          isT <- isTypeToken
          if isT
            then do
              ty <- parseTypeName
              expect TkRParen
              pure (mkExpr loc (ESizeofType ty))
            else do
              inner <- parseExpr
              expect TkRParen
              pure (mkExpr loc (ESizeofExpr inner))
        else mkExpr loc . ESizeofExpr <$> parseUnary
    _ -> parsePostfixExpr
  where
    pre loc op = do
      advance
      mkExpr loc . EUnary op <$> parseUnary

binOpOf :: TokenKind -> Maybe (BinOp, Int)
binOpOf k = case k of
  TkOrOr -> Just (LOr, 1)
  TkAndAnd -> Just (LAnd, 2)
  TkPipe -> Just (BOr, 3)
  TkCaret -> Just (BXor, 4)
  TkAmp -> Just (BAnd, 5)
  TkEq -> Just (Eq, 6)
  TkNe -> Just (Ne, 6)
  TkLt -> Just (Lt, 7)
  TkGt -> Just (Gt, 7)
  TkLe -> Just (Le, 7)
  TkGe -> Just (Ge, 7)
  TkLShift -> Just (Shl, 8)
  TkRShift -> Just (Shr, 8)
  TkPlus -> Just (Add, 9)
  TkMinus -> Just (Sub, 9)
  TkStar -> Just (Mul, 10)
  TkSlash -> Just (Div, 10)
  TkPercent -> Just (Mod, 10)
  _ -> Nothing

binPrec :: TokenKind -> Int
binPrec = maybe (-1) snd . binOpOf

parseBinRhs :: Expr -> Int -> P Expr
parseBinRhs lhs minPrec = do
  t <- cur
  case binOpOf (tokKind t) of
    Just (op, prec) | prec >= minPrec -> do
      advance
      rhs0 <- parseUnary
      nextPrec <- binPrec <$> curKind
      rhs <- if nextPrec > prec then parseBinRhs rhs0 (prec + 1) else pure rhs0
      parseBinRhs (mkExpr (tokLoc t) (EBinary op lhs rhs)) minPrec
    _ -> pure lhs

parseCond :: P Expr
parseCond = do
  e <- parseUnary >>= \l -> parseBinRhs l 0
  q <- match TkQuestion
  if not q
    then pure e
    else do
      th <- parseExpr
      expect TkColon
      el <- parseCond
      pure (mkExpr (exLoc e) (ECond e th el))

assignOpOf :: TokenKind -> Maybe AssignOp
assignOpOf k = case k of
  TkAssign -> Just Assign
  TkAddAssign -> Just (AssignOp Add)
  TkSubAssign -> Just (AssignOp Sub)
  TkMulAssign -> Just (AssignOp Mul)
  TkDivAssign -> Just (AssignOp Div)
  TkModAssign -> Just (AssignOp Mod)
  TkAndAssign -> Just (AssignOp BAnd)
  TkOrAssign -> Just (AssignOp BOr)
  TkXorAssign -> Just (AssignOp BXor)
  TkLShiftAssign -> Just (AssignOp Shl)
  TkRShiftAssign -> Just (AssignOp Shr)
  _ -> Nothing

parseAssign :: P Expr
parseAssign = do
  e <- parseCond
  t <- cur
  case assignOpOf (tokKind t) of
    Nothing -> pure e
    Just op -> do
      advance
      mkExpr (tokLoc t) . EAssign op e <$> parseAssign

parseExpr :: P Expr
parseExpr = parseAssign >>= go
  where
    go e = do
      c <- match TkComma
      if not c
        then pure e
        else do
          r <- parseAssign
          go (mkExpr (exLoc e) (EComma e r))

-- ---- constant folding ----

-- | Fold an already-parsed expression to an integer constant (C99 6.6): array
-- bounds that fold are fixed arrays, the rest are VLAs.
foldConst :: Expr -> P (Maybe Integer)
foldConst e = case exNode e of
  EInt v _ -> pure (Just v)
  EChar v -> pure (Just v)
  EIdent n _ -> lookupEnum n
  EUnary op a -> do
    v <- foldConst a
    pure $ case (op, v) of
      (_, Nothing) -> Nothing
      (Neg, Just x) -> Just (negate x)
      (UPlus, Just x) -> Just x
      (BNot, Just x) -> Just (complement x)
      (Not, Just x) -> Just (boolOf (x == 0))
      _ -> Nothing
  EBinary op a b -> do
    va <- foldConst a
    vb <- foldConst b
    pure $ case (va, vb) of
      (Just x, Just y) -> foldBin op x y
      _ -> Nothing
  ECond c a b -> do
    vc <- foldConst c
    case vc of
      Nothing -> pure Nothing
      Just x -> foldConst (if x /= 0 then a else b)
  ECast ty a -> do
    v <- foldConst a
    tc <- gets psTc
    pure $ case v of
      Nothing -> Nothing
      Just x -> Just (narrow tc ty x)
  ESizeofType ty -> do
    tc <- gets psTc
    let sz = typeSize tc ty
    pure $ if typeIsComplete tc ty && sz > 0 then Just (toInteger sz) else Nothing
  _ -> pure Nothing
  where
    boolOf b = if b then 1 else 0
    foldBin op x y = case op of
      Add -> Just (x + y)
      Sub -> Just (x - y)
      Mul -> Just (x * y)
      Div -> if y == 0 then Nothing else Just (x `quot` y)
      Mod -> if y == 0 then Nothing else Just (x `rem` y)
      Eq -> Just (boolOf (x == y))
      Ne -> Just (boolOf (x /= y))
      Lt -> Just (boolOf (x < y))
      Le -> Just (boolOf (x <= y))
      Gt -> Just (boolOf (x > y))
      Ge -> Just (boolOf (x >= y))
      LAnd -> Just (boolOf (x /= 0 && y /= 0))
      LOr -> Just (boolOf (x /= 0 || y /= 0))
      BAnd -> Just (x .&. y)
      BOr -> Just (x .|. y)
      BXor -> Just (x `xor` y)
      Shl -> Just (x `shiftL` fromInteger (y .&. 63))
      Shr -> Just (x `shiftR` fromInteger (y .&. 63))
    narrow tc ty x
      | typeIsInteger ty, sz > 0, sz < 8 =
          if typeIsUnsigned ty
            then x .&. (bit1 - 1)
            else
              let m = x .&. (bit1 - 1)
               in if m >= bit1 `shiftR` 1 then m - bit1 else m
      | otherwise = x
      where
        sz = typeSize tc ty
        bit1 = 1 `shiftL` (8 * sz)

-- ---- integer constant expressions (enumerator values) ----

parseConstPrimary :: P Integer
parseConstPrimary = do
  t <- cur
  case tokKind t of
    TkInt -> advance >> pure (tokIVal t)
    TkChar -> advance >> pure (tokIVal t)
    TkLParen -> do
      advance
      v <- parseConstOr
      expect TkRParen
      pure v
    TkIdent -> do
      v <- lookupEnum (tokText t)
      advance
      pure (fromMaybe 0 v)
    _ -> do
      perr (tokLoc t) "expected integer constant expression"
      advance
      pure 0

parseConstUnary :: P Integer
parseConstUnary = do
  k <- curKind
  case k of
    TkPlus -> advance >> parseConstUnary
    TkMinus -> advance >> (negate <$> parseConstUnary)
    TkTilde -> advance >> (complement <$> parseConstUnary)
    TkBang -> advance >> (asBool . (== 0) <$> parseConstUnary)
    _ -> parseConstPrimary
  where
    asBool b = if b then 1 else 0

parseConstMul :: P Integer
parseConstMul = parseConstUnary >>= go
  where
    go v = do
      k <- curKind
      case k of
        TkStar -> advance >> parseConstUnary >>= \r -> go (v * r)
        TkSlash -> advance >> parseConstUnary >>= \r -> go (if r == 0 then 0 else v `quot` r)
        TkPercent -> advance >> parseConstUnary >>= \r -> go (if r == 0 then 0 else v `rem` r)
        _ -> pure v

parseConstAdd :: P Integer
parseConstAdd = parseConstMul >>= go
  where
    go v = do
      k <- curKind
      case k of
        TkPlus -> advance >> parseConstMul >>= \r -> go (v + r)
        TkMinus -> advance >> parseConstMul >>= \r -> go (v - r)
        _ -> pure v

parseConstShift :: P Integer
parseConstShift = parseConstAdd >>= go
  where
    go v = do
      k <- curKind
      case k of
        TkLShift -> advance >> parseConstAdd >>= \r -> go (v `shiftL` fromInteger (r .&. 63))
        TkRShift -> advance >> parseConstAdd >>= \r -> go (v `shiftR` fromInteger (r .&. 63))
        _ -> pure v

parseConstOr :: P Integer
parseConstOr = parseConstShift >>= go
  where
    go v = do
      k <- curKind
      case k of
        TkAmp -> advance >> parseConstShift >>= \r -> go (v .&. r)
        TkCaret -> advance >> parseConstShift >>= \r -> go (v `xor` r)
        TkPipe -> advance >> parseConstShift >>= \r -> go (v .|. r)
        _ -> pure v

-- ---- types ----

optIdentName :: P (Maybe String)
optIdentName = do
  t <- cur
  if tokKind t == TkIdent
    then advance >> pure (Just (tokText t))
    else pure Nothing

parseStructOrUnion :: Bool -> P Type
parseStructOrUnion isUnion = do
  advance -- struct / union
  mtag <- optIdentName
  let kind = if isUnion then KUnion else KStruct
  tid <- declareTag kind mtag
  brace <- match TkLBrace
  when brace $ do
    ms <- memberLoop []
    expect TkRBrace
    setMembers tid ms
  pure (TStruct tid)
  where
    memberLoop acc = do
      k <- curKind
      if k == TkRBrace || k == TkEof
        then pure (reverse acc)
        else do
          before <- gets psPos
          loc <- curLoc
          (base, _, saw, _) <- parseDeclSpecs
          if not saw
            then do
              perr loc "expected member declaration"
              pure (reverse acc)
            else do
              ms <- declaratorLoop base
              expect TkSemi
              after <- gets psPos
              when (after == before) advance
              memberLoop (reverse ms ++ acc)

    declaratorLoop base = do
      m <- oneMember base
      more <- match TkComma
      rest <- if more then declaratorLoop base else pure []
      pure (maybe rest (: rest) m)

    oneMember base = do
      (ty, mname, _) <- parseDeclarator base
      colon <- match TkColon
      if colon
        then do
          w <- bitFieldWidth
          pure (Just (Member mname ty 0 (Just (0, w))))
        else case mname of
          Just n -> pure (Just (Member (Just n) ty 0 Nothing))
          Nothing -> case ty of
            -- anonymous struct/union member (C11 6.7.2.1p13)
            TStruct _ -> pure (Just (Member Nothing ty 0 Nothing))
            _ -> do
              loc <- curLoc
              perr loc "expected member name"
              pure Nothing

    bitFieldWidth = do
      t <- cur
      if tokKind t == TkInt
        then advance >> pure (fromInteger (tokIVal t))
        else do
          perr (tokLoc t) "expected bit-field width"
          pure 0

parseEnum :: P (Type, [(String, Integer)])
parseEnum = do
  advance -- enum
  mtag <- optIdentName
  tid <- declareTag KEnum mtag
  brace <- match TkLBrace
  if not brace
    then pure (TEnum tid, [])
    else do
      es <- enumLoop 0 []
      expect TkRBrace
      pure (TEnum tid, es)
  where
    enumLoop val acc = do
      k <- curKind
      if k == TkRBrace || k == TkEof
        then pure (reverse acc)
        else do
          idt <- cur
          expect TkIdent
          hasVal <- match TkAssign
          v <- if hasVal then parseConstOr else pure val
          let name = tokText idt
          registerEnum name v
          more <- match TkComma
          let acc' = (name, v) : acc
          if more then enumLoop (v + 1) acc' else pure (reverse acc')

-- | @unsigned __int128@ desugars to a two-u64 struct; sema rewrites its
-- operators into __c99m_u128_* helper calls.
u128Type :: P Type
u128Type = do
  tc <- gets psTc
  case tagLookup tc KStruct u128Name of
    Just tid | not (tagIncomplete (tagInfo tc tid)) -> pure (TStruct tid)
    _ -> do
      tid <- declareTag KStruct (Just u128Name)
      setMembers
        tid
        [ Member (Just "lo") TULLong 0 Nothing
        , Member (Just "hi") TULLong 0 Nothing
        ]
      pure (TStruct tid)
  where
    u128Name = "__c99m_u128"

data BaseSpec = BsNone | BsVoid | BsChar | BsInt | BsFloat | BsDouble | BsBool | BsTag
  deriving (Eq)

data Specs = Specs
  { spBase :: !BaseSpec
  , spTag :: Maybe Type
  , spUnsigned :: !Bool
  , spSigned :: !Bool
  , spLong :: !Int
  , spShort :: !Int
  , spComplex :: !Bool
  , spSc :: !StorageClass
  , spSaw :: !Bool
  , spEnums :: [(String, Integer)]
  }

-- | Returns the base type, the storage class, whether any type specifier was
-- seen at all, and the enumerators of an enum defined here.
parseDeclSpecs :: P (Type, StorageClass, Bool, [(String, Integer)])
parseDeclSpecs = go (Specs BsNone Nothing False False 0 0 False ScNone False [])
  where
    go sp = do
      k <- curKind
      case k of
        TkTypedef -> advance >> go sp {spSc = ScTypedef}
        TkExtern -> advance >> go sp {spSc = ScExtern}
        TkStatic -> advance >> go sp {spSc = ScStatic}
        TkAuto -> advance >> go sp
        TkRegister -> advance >> go sp
        TkInline -> advance >> go sp
        TkConst -> advance >> go sp
        TkVolatile -> advance >> go sp
        TkRestrict -> advance >> go sp
        TkVoid -> advance >> go sp {spBase = BsVoid, spSaw = True}
        TkCharKw -> advance >> go sp {spBase = BsChar, spSaw = True}
        TkIntKw -> advance >> go sp {spBase = BsInt, spSaw = True}
        TkFloatKw -> advance >> go sp {spBase = BsFloat, spSaw = True}
        TkDouble -> advance >> go sp {spBase = BsDouble, spSaw = True}
        TkBool -> advance >> go sp {spBase = BsBool, spSaw = True}
        TkComplex -> advance >> go sp {spComplex = True, spSaw = True}
        TkShort -> advance >> go sp {spShort = spShort sp + 1, spSaw = True}
        TkLong -> advance >> go sp {spLong = spLong sp + 1, spSaw = True}
        TkSigned -> advance >> go sp {spSigned = True, spSaw = True}
        TkUnsigned -> advance >> go sp {spUnsigned = True, spSaw = True}
        TkStruct -> do
          t <- parseStructOrUnion False
          go sp {spBase = BsTag, spTag = Just t, spSaw = True}
        TkUnion -> do
          t <- parseStructOrUnion True
          go sp {spBase = BsTag, spTag = Just t, spSaw = True}
        TkEnum -> do
          (t, es) <- parseEnum
          go sp {spBase = BsTag, spTag = Just t, spSaw = True, spEnums = spEnums sp ++ es}
        TkInt128 -> do
          advance
          modify' $ \s -> s {psSawInt128 = True}
          t <- u128Type
          go sp {spBase = BsTag, spTag = Just t, spSaw = True}
        TkIdent -> do
          t <- cur
          mty <- lookupTypedef (tokText t)
          case mty of
            Nothing -> finish sp
            Just ty -> do
              advance
              go sp {spBase = BsTag, spTag = Just ty, spSaw = True}
        _ -> finish sp

    finish sp = pure (complexify sp (baseTy sp), spSc sp, spSaw sp, spEnums sp)

    baseTy sp = case spBase sp of
      BsNone -> intLike sp
      BsInt -> intLike sp
      BsVoid -> TVoid
      BsBool -> TBool
      BsChar
        | spUnsigned sp -> TUChar
        | spSigned sp -> TSChar
        | otherwise -> TChar
      BsFloat -> TFloat
      BsDouble -> if spLong sp > 0 then TLDouble else TDouble
      BsTag -> fromMaybe TInt (spTag sp)

    intLike sp
      | spShort sp > 0 = if u then TUShort else TShort
      | spLong sp >= 2 = if u then TULLong else TLLong
      | spLong sp == 1 = if u then TULong else TLong
      | otherwise = if u then TUInt else TInt
      where
        u = spUnsigned sp

    complexify sp ty
      | spComplex sp = TComplex (if spBase sp == BsFloat then TFloat else TDouble)
      | otherwise = ty

-- ---- declarators ----

parsePointers :: Type -> P Type
parsePointers base = do
  star <- match TkStar
  if not star
    then pure base
    else do
      skipQuals
      parsePointers (TPtr base)
  where
    skipQuals = do
      k <- curKind
      when (k == TkConst || k == TkVolatile || k == TkRestrict) (advance >> skipQuals)

-- | How many pointer layers sit on @t@ above @root@.
ptrDepthAbove :: Type -> Type -> Int
ptrDepthAbove = go (0 :: Int)
  where
    go d t root = case t of
      TPtr b | t /= root && d <= 64 -> go (d + 1) b root
      _ -> d

applyPtrs :: Int -> Type -> Type
applyPtrs n t = iterate TPtr t !! max 0 n

-- | (type, declared name, parameters of a function suffix)
type DeclResult = (Type, Maybe String, Maybe [Param])

parseDeclarator :: Type -> P DeclResult
parseDeclarator base = do
  savedOn <- gets psVlaOn
  savedSz <- gets psVlaSize
  modify' $ \s -> s {psVlaOn = False}
  r <- parsePointers base >>= parseDirectDeclarator
  modify' $ \s -> s {psVlaOn = savedOn, psVlaSize = savedSz}
  pure r

-- | As 'parseDeclarator', but also reports the bound of a VLA that did not fold
-- to a constant.
parseDeclaratorVla :: Type -> P (Type, Maybe String, Maybe [Param], Maybe Expr)
parseDeclaratorVla base = do
  savedOn <- gets psVlaOn
  savedSz <- gets psVlaSize
  modify' $ \s -> s {psVlaOn = True, psVlaSize = Nothing}
  (ty, nm, ps) <- parsePointers base >>= parseDirectDeclarator
  sz <- gets psVlaSize
  modify' $ \s -> s {psVlaOn = savedOn, psVlaSize = savedSz}
  pure (ty, nm, ps, sz)

parseDirectDeclarator :: Type -> P DeclResult
parseDirectDeclarator base = do
  lp <- match TkLParen
  if not lp
    then plain
    else do
      k <- curKind
      if k == TkStar || k == TkIdent || k == TkLParen
        then do
          -- Parenthesized declarator: the postfix suffixes outside the parens
          -- apply to the base type first, and the pointer layers from the
          -- nested declarator wrap the result. That is what makes
          -- `int (*fp)(int)` a pointer to a function rather than a function
          -- returning a pointer.
          (inner, nm, ps1) <- parseDeclarator base
          expect TkRParen
          let stars = ptrDepthAbove inner base
          (post, ps2) <- parsePostfixDeclarator base
          pure (applyPtrs stars post, nm, ps2 <|> ps1)
        else do
          expect TkRParen
          plain
  where
    plain = do
      nm <- optIdentName
      (ty, ps) <- parsePostfixDeclarator base
      pure (ty, nm, ps)

parsePostfixDeclarator :: Type -> P (Type, Maybe [Param])
parsePostfixDeclarator base = do
  k <- curKind
  case k of
    TkLParen -> do
      advance
      (ft, ps1) <- parseParamList base
      (ty, ps2) <- parsePostfixDeclarator ft
      pure (ty, ps2 <|> ps1)
    TkLBracket -> do
      advance
      skipArrayQuals
      (len, isVla) <- parseArrayBound
      expect TkRBracket
      -- `int a[3][4]` is array[3] of array[4] of int: the leftmost bracket is
      -- the outermost array, so the suffixes to the right are applied first.
      (elemTy, ps) <- parsePostfixDeclarator base
      pure (TArray elemTy len isVla, ps)
    _ -> pure (base, Nothing)
  where
    skipArrayQuals = do
      k <- curKind
      when (k == TkStatic || k == TkConst || k == TkVolatile || k == TkRestrict) $
        advance >> skipArrayQuals

parseArrayBound :: P (Int, Bool)
parseArrayBound = do
  k <- curKind
  n <- peekKind
  if k == TkStar && n == TkRBracket
    then advance >> pure (0, True) -- [*]: unspecified VLA bound
    else
      if k == TkRBracket
        then pure (0, False)
        else do
          bound <- parseAssign
          v <- foldConst bound
          case v of
            Just c | c >= 0 -> pure (fromInteger c, False)
            _ -> do
              on <- gets psVlaOn
              sz <- gets psVlaSize
              when (on && isNothing sz) $ modify' $ \s -> s {psVlaSize = Just bound}
              pure (0, True)

parseParamList :: Type -> P (Type, Maybe [Param])
parseParamList ret = do
  k <- curKind
  n <- peekKind
  if k == TkVoid && n == TkRParen
    then do
      advance
      expect TkRParen
      pure (TFunc ret [] False False, Just [])
    else do
      rp <- match TkRParen
      if rp
        then pure (TFunc ret [] False True, Just []) -- `()`: unspecified params
        else loop []
  where
    loop acc = do
      ell <- match TkEllipsis
      if ell
        then finish acc True
        else do
          (bs, _, saw, _) <- parseDeclSpecs
          (pt0, pname, _) <- parseDeclarator (if saw then bs else TInt)
          let p = Param (fromMaybe "" pname) (typeDecay pt0) Nothing
          more <- match TkComma
          if more then loop (p : acc) else finish (p : acc) False

    finish acc variadic = do
      expect TkRParen
      let ps = reverse acc
      pure (TFunc ret (map pType ps) variadic False, Just ps)

parseAbstractDeclarator :: Type -> P Type
parseAbstractDeclarator base0 = do
  base <- parsePointers base0
  lp <- match TkLParen
  if not lp
    then fst <$> parsePostfixDeclarator base
    else do
      isT <- isTypeToken
      rp <- check TkRParen
      if isT || rp
        then do
          (ft, _) <- parseParamList base
          fst <$> parsePostfixDeclarator ft
        else do
          inner <- parseAbstractDeclarator base
          expect TkRParen
          let stars = ptrDepthAbove inner base
          (post, _) <- parsePostfixDeclarator (if stars == 0 then inner else base)
          pure (applyPtrs stars post)

parseTypeName :: P Type
parseTypeName = do
  (base, _, saw, _) <- parseDeclSpecs
  parseAbstractDeclarator (if saw then base else TInt)

-- ---- initializers ----

parseInitializer :: P Init
parseInitializer = do
  brace <- match TkLBrace
  if not brace
    then IExpr <$> parseAssign
    else do
      rb <- check TkRBrace
      items <- if rb then pure [] else itemLoop
      expect TkRBrace
      pure (IList items)
  where
    itemLoop = do
      rb <- check TkRBrace
      if rb
        then pure []
        else do
          item <- oneItem
          more <- match TkComma
          if more then (item :) <$> itemLoop else pure [item]

    oneItem = do
      k <- curKind
      case k of
        TkDot -> do
          advance
          idt <- cur
          expect TkIdent
          expect TkAssign
          i <- parseInitializer
          pure (Just (DField (tokText idt)), i)
        TkLBracket -> do
          advance
          idx <- parseCond
          expect TkRBracket
          expect TkAssign
          i <- parseInitializer
          pure (Just (DIndex idx), i)
        _ -> do
          i <- parseInitializer
          pure (Nothing, i)

-- ---- statements ----

parseCompound :: P Stmt
parseCompound = do
  loc <- curLoc
  expect TkLBrace
  items <- loop []
  expect TkRBrace
  pure (Stmt loc (SCompound (reverse items)))
  where
    loop acc = do
      k <- curKind
      if k == TkRBrace || k == TkEof
        then pure acc
        else do
          before <- gets psPos
          i <- parseDeclOrStmt
          after <- gets psPos
          when (after == before) advance
          loop (i : acc)

parseStmt :: P Stmt
parseStmt = do
  t <- cur
  let loc = tokLoc t
  case tokKind t of
    TkLBrace -> parseCompound
    TkIf -> do
      advance
      expect TkLParen
      c <- parseExpr
      expect TkRParen
      th <- parseStmt
      hasElse <- match TkElse
      el <- if hasElse then Just <$> parseStmt else pure Nothing
      pure (Stmt loc (SIf c th el))
    TkWhile -> do
      advance
      expect TkLParen
      c <- parseExpr
      expect TkRParen
      body <- parseStmt
      pure (Stmt loc (SWhile c body))
    TkDo -> do
      advance
      body <- parseStmt
      expect TkWhile
      expect TkLParen
      c <- parseExpr
      expect TkRParen
      expect TkSemi
      pure (Stmt loc (SDo body c))
    TkFor -> parseFor loc
    TkSwitch -> do
      advance
      expect TkLParen
      c <- parseExpr
      expect TkRParen
      body <- parseStmt
      pure (Stmt loc (SSwitch c body))
    TkBreak -> do
      advance
      expect TkSemi
      pure (Stmt loc SBreak)
    TkContinue -> do
      advance
      expect TkSemi
      pure (Stmt loc SContinue)
    TkReturn -> do
      advance
      semi <- check TkSemi
      e <- if semi then pure Nothing else Just <$> parseExpr
      expect TkSemi
      pure (Stmt loc (SReturn e))
    TkGoto -> do
      advance
      idt <- cur
      expect TkIdent
      expect TkSemi
      pure (Stmt loc (SGoto (tokText idt)))
    TkCase -> do
      advance
      v <- parseCond
      expect TkColon
      body <- parseStmt
      pure (Stmt loc (SCase v body))
    TkDefault -> do
      advance
      expect TkColon
      body <- parseStmt
      pure (Stmt loc (SDefault body))
    TkSemi -> advance >> pure (Stmt loc SNull)
    TkIdent -> do
      n <- peekKind
      if n == TkColon
        then do
          advance
          advance
          body <- parseStmt
          pure (Stmt loc (SLabel (tokText t) body))
        else exprStmt loc
    _ -> exprStmt loc
  where
    exprStmt loc = do
      e <- parseExpr
      expect TkSemi
      pure (Stmt loc (SExpr e))

parseFor :: SrcLoc -> P Stmt
parseFor loc = do
  advance
  expect TkLParen
  semi <- check TkSemi
  ini <-
    if semi
      then expect TkSemi >> pure Nothing
      else do
        isT <- isTypeToken
        if isT
          then Just <$> parseDeclOrStmt
          else do
            iloc <- curLoc
            e <- parseExpr
            expect TkSemi
            pure (Just (BIStmt (Stmt iloc (SExpr e))))
  semi2 <- check TkSemi
  c <- if semi2 then pure Nothing else Just <$> parseExpr
  expect TkSemi
  rp <- check TkRParen
  inc <- if rp then pure Nothing else Just <$> parseExpr
  expect TkRParen
  body <- parseStmt
  pure (Stmt loc (SFor ini c inc body))

-- ---- declarations ----

-- | GCC asm label: @int f(void) __asm__("real_name");@
parseAsmLabel :: P (Maybe String)
parseAsmLabel = do
  t <- cur
  if tokKind t == TkIdent && (tokText t == "__asm__" || tokText t == "__asm")
    then do
      advance
      expect TkLParen
      s <- cur
      lbl <-
        if tokKind s == TkString
          then advance >> pure (Just (tokText s))
          else pure Nothing
      expect TkRParen
      pure lbl
    else pure Nothing

-- | The declarator list of one declaration: everything after the specifiers, up
-- to but not including the ';'.
declSeq :: Bool -> SrcLoc -> Type -> StorageClass -> (Type, Maybe String, Maybe Expr) -> P [Decl]
declSeq wantVla loc base sc first = go first []
  where
    go (ty, mname, vla) acc = do
      let name = fromMaybe "" mname
      when (sc == ScTypedef) $ registerTypedef name ty
      lbl <- parseAsmLabel
      hasInit <- match TkAssign
      ini <- if hasInit then Just <$> parseInitializer else pure Nothing
      let d =
            Decl
              { dLoc = loc
              , dName = name
              , dType = ty
              , dStorage = sc
              , dInit = ini
              , dAsmLabel = lbl
              , dVlaSize = vla
              , dSym = Nothing
              }
      more <- match TkComma
      if not more
        then pure (reverse (d : acc))
        else do
          nxt <- nextDeclarator
          go nxt (d : acc)

    nextDeclarator
      | wantVla = do
          (ty, nm, _, vla) <- parseDeclaratorVla base
          pure (ty, nm, vla)
      | otherwise = do
          (ty, nm, _) <- parseDeclarator base
          pure (ty, nm, Nothing)

isFuncType :: Type -> Bool
isFuncType TFunc {} = True
isFuncType _ = False

funcIsVariadic :: Type -> Bool
funcIsVariadic (TFunc _ _ v _) = v
funcIsVariadic _ = False

enumConsts :: Type -> [(String, Integer)] -> [TopDecl]
enumConsts ty = map (\(n, v) -> TDEnumConst n v ty)

parseDeclOrStmt :: P BlockItem
parseDeclOrStmt = do
  isT <- isTypeToken
  k <- curKind
  if not (isT || k `elem` [TkTypedef, TkExtern, TkStatic, TkAuto, TkRegister, TkInline])
    then BIStmt <$> parseStmt
    else do
      loc <- curLoc
      (base, sc, saw, es) <- parseDeclSpecs
      if not saw && sc == ScNone
        then BIStmt <$> parseStmt
        else do
          semi <- check TkSemi
          if semi
            then do
              advance
              -- A block-scope `enum { A, B };` introduces its enumerators; the
              -- AST can only hold them at the top level.
              pushPending (enumConsts base es)
              pure (BIStmt (Stmt loc SNull))
            else do
              (ty, mname, _, vla) <- parseDeclaratorVla base
              brace <- check TkLBrace
              if isFuncType ty && brace
                then do
                  when (sc == ScTypedef) $ perr loc "typedef function definition"
                  perr loc "nested function definition"
                  body <- parseCompound
                  pure (BIStmt body)
                else do
                  pushPending (enumConsts base es)
                  ds <- declSeq True loc base sc (ty, mname, vla)
                  expect TkSemi
                  pure (BIDecl ds)

program :: P Program
program = go
  where
    go = do
      k <- curKind
      if k == TkEof
        then pure []
        else do
          before <- gets psPos
          ds <- topDecl
          after <- gets psPos
          when (after == before) advance
          pending <- takePending
          rest <- go
          pure (pending ++ ds ++ rest)

topDecl :: P [TopDecl]
topDecl = do
  isT <- isTypeToken
  k <- curKind
  loc <- curLoc
  if not (isT || k `elem` [TkTypedef, TkExtern, TkStatic, TkInline])
    then do
      perr loc "expected declaration"
      advance
      pure []
    else do
      (base, sc, _, es) <- parseDeclSpecs
      semi <- check TkSemi
      if semi
        then do
          advance
          pure (tagDecl base ++ enumConsts base es)
        else do
          (ty, mname, mparams) <- parseDeclarator base
          brace <- check TkLBrace
          if isFuncType ty && brace
            then do
              when (sc == ScTypedef) $ perr loc "typedef function definition"
              body <- parseCompound
              let fd =
                    FuncDef
                      { fdLoc = loc
                      , fdName = fromMaybe "" mname
                      , fdType = ty
                      , fdParams = fromMaybe [] mparams
                      , fdStorage = sc
                      , fdVariadic = funcIsVariadic ty
                      , fdBody = body
                      , fdSym = Nothing
                      }
              pure (enumConsts base es ++ [TDFunc fd])
            else do
              ds <- declSeq False loc base sc (ty, mname, Nothing)
              expect TkSemi
              pure (enumConsts base es ++ map TDDecl ds)
  where
    tagDecl base = case base of
      TStruct tid -> [TDTag tid]
      TEnum tid -> [TDTag tid]
      _ -> []
