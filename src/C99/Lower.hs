-- | Lowering the checked AST to libmtlc IR (src/lower.c).
--
-- Runs in IO because the IR builder is a C object being mutated. The order of
-- emission is therefore load-bearing: labels, branches and stores come out in
-- the order this code runs them.
--
-- Three representation choices drive most of what follows, and all three are
-- forced by what the public builder (mtlc/build.h) can express:
--
--   * C aggregates (struct, union, array, _Complex) have no IR type. They live
--     in memory and are passed around as @uint8*@ addresses; an "aggregate
--     rvalue" is its address, never a loaded value.
--   * A file-scope aggregate — or any global whose address is taken — becomes a
--     /pointer/ global, allocated on the heap by a generated constructor, since
--     mtlc_address_of is only defined for locals and parameters.
--   * String literals are packed into u64 data globals and copied once, on
--     first use, into a heap buffer held by a pointer global.
module C99.Lower
  ( lowerProgram
  ) where

import Control.Monad (foldM, forM, forM_, unless, when)
import Control.Monad.State.Strict
import Data.Bits (bit, complement, shiftL)
import Data.Char (ord)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import Data.Word (Word32, Word64)

import C99.Ast
import C99.Common
import C99.CType
import C99.Sema (SemaResult (..), foldConst)
import Mtlc

-- | Every call to an extern variadic pads its tail to this arity, so all call
-- sites agree with the one module-level signature. Win64 varargs read argument
-- slots from memory, so the extra zero i64 arguments are harmless.
variadicPad :: Int
variadicPad = 32

data LowerState = LowerState
  { lsBuilder :: Builder
  , lsFn :: Fn
  , lsTc :: TypeContext
  , lsSyms :: M.Map SymId Symbol
  , lsLabelId :: !Int
  , lsTmpId :: !Int
  , lsStrId :: !Int
  , -- | Scalar locals and parameters: the value handle is the storage.
    lsLocals :: M.Map SymId Value
  , -- | Aggregate locals: the handle holds the object's base address. Reading
    -- one yields the address, never a load through it.
    lsPtrs :: M.Map SymId Value
  , -- | Interned string literals: content -> name of its pointer global.
    lsStrings :: M.Map String String
  , -- | Pointer globals already lazily-initialized in the current function.
    -- Functions run in arbitrary order, so this must be per-function: relying
    -- on the module-wide first use leaves the pointer null in earlier callers.
    lsFnStrEnsured :: [String]
  , lsLoop :: [(String, String)] -- (break label, continue label)
  , -- | One entry per open switch, innermost first: the IR label for each case
    -- value, and the default's label. A case label is emitted where it occurs
    -- rather than collected from the top level of the body, so a case inside a
    -- nested block or a loop is reachable.
    lsSwitch :: [(M.Map Integer String, Maybe String)]
  , lsRetType :: Maybe Type
  , lsEmittedReturn :: !Bool
  , lsVaParam :: Maybe Value
  , lsSretParam :: Maybe Value
  , lsDeclaredExterns :: [String]
  , lsGlobalInits :: [Decl] -- file-scope decls needing the constructor
  , lsAggGlobals :: [(String, Int)] -- addressable globals: link name, bytes
  , lsNeedGlobalInit :: !Bool
  , lsMsgs :: [Message]
  }

type Lower a = StateT LowerState IO a

-- | Lower a checked program to a module. Nothing on builder failure.
lowerProgram :: SemaResult -> IO (Maybe Module, [Message])
lowerProgram sr = do
  b <- builderCreate
  declareRuntime b
  let st0 =
        LowerState
          { lsBuilder = b
          , lsFn = error "no current function"
          , lsTc = srTc sr
          , lsSyms = srSyms sr
          , lsLabelId = 0
          , lsTmpId = 0
          , lsStrId = 0
          , lsLocals = M.empty
          , lsPtrs = M.empty
          , lsStrings = M.empty
          , lsFnStrEnsured = []
          , lsLoop = []
          , lsSwitch = []
          , lsRetType = Nothing
          , lsEmittedReturn = False
          , lsVaParam = Nothing
          , lsSretParam = Nothing
          , lsDeclaredExterns = []
          , lsGlobalInits = []
          , lsAggGlobals = []
          , lsNeedGlobalInit = False
          , lsMsgs = []
          }
  (mmod, st) <- runStateT (run (srProgram sr)) st0
  pure (mmod, reverse (lsMsgs st))
  where
    run prog = do
      mapM_ declareVar (globalVarsOf prog)
      emitGlobalCtor
      forM_ prog $ \td -> case td of
        TDFunc fd -> genFunction fd
        _ -> pure ()
      b <- gets lsBuilder
      m <- lift (builderFinish b)
      when (m == Nothing) $ err noLoc "IR builder failed"
      pure m

err :: SrcLoc -> String -> Lower ()
err loc text = modify' $ \s -> s {lsMsgs = diag Error loc text : lsMsgs s}

-- ---- naming ----

freshLabel :: String -> Lower String
freshLabel prefix = do
  n <- gets lsLabelId
  modify' $ \s -> s {lsLabelId = n + 1}
  pure (".L" ++ prefix ++ show n)

freshTmp :: String -> Lower String
freshTmp prefix = do
  n <- gets lsTmpId
  modify' $ \s -> s {lsTmpId = n + 1}
  pure (prefix ++ "_" ++ show n)

-- ---- types ----

-- | The IR type a C type lowers to. Aggregates and _Complex become @uint8*@:
-- they are addresses, and their size is the caller's business.
mtlcOf :: Type -> Lower Ty
mtlcOf t = lift (mtlcOfIO t)

mtlcOfIO :: Type -> IO Ty
mtlcOfIO t = case t of
  TVoid -> tyScalar Void
  TBool -> tyScalar Bool
  TChar -> tyScalar I8
  TSChar -> tyScalar I8
  TUChar -> tyScalar U8
  TShort -> tyScalar I16
  TUShort -> tyScalar U16
  TInt -> tyScalar I32
  TEnum _ -> tyScalar I32
  TUInt -> tyScalar U32
  TLong -> tyScalar I32 -- LLP64
  TULong -> tyScalar U32
  TLLong -> tyScalar I64
  TULLong -> tyScalar U64
  TFloat -> tyScalar F32
  TDouble -> tyScalar F64
  TLDouble -> tyScalar F64
  TFunc {} -> tyScalar I64
  TPtr b -> case b of
    TFunc {} -> tyScalar I64 -- a code address, held as an integer
    TVoid -> i8p
    TStruct _ -> i8p
    TComplex _ -> i8p
    _ -> mtlcOfIO b >>= tyPointer
  TArray b _ _ -> mtlcOfIO b >>= tyPointer
  TStruct _ -> i8p
  TComplex _ -> i8p

i8p :: IO Ty
i8p = tyScalar U8 >>= tyPointer

i8pL :: Lower Ty
i8pL = lift i8p

u64L :: Lower Ty
u64L = lift (tyScalar U64)

i64L :: Lower Ty
i64L = lift (tyScalar I64)

i32L :: Lower Ty
i32L = lift (tyScalar I32)

f64L :: Lower Ty
f64L = lift (tyScalar F64)

isAgg :: Type -> Bool
isAgg = typeIsAggregate

sizeOf :: Type -> Lower Int
sizeOf t = do
  tc <- gets lsTc
  pure (typeSize tc t)

symOf :: SymId -> Lower Symbol
symOf sid = gets ((M.! sid) . lsSyms)

-- | A conversion that the IR needs. Aggregates and decays are addresses
-- already, so they pass through untouched.
castTo :: Value -> Type -> Type -> Lower Value
castTo v from to
  | typeEqual from to = pure v
  | TArray {} <- from, TPtr _ <- to = pure v
  | TFunc {} <- from, TPtr _ <- to = pure v
  | isAgg from || isAgg to = pure v
  | otherwise = do
      fn <- gets lsFn
      ty <- mtlcOf to
      lift (cast fn v ty)

-- | Advance a pointer by @iv@ /elements/, yielding a value of type @resTy@.
--
-- libmtlc's 'binary' does no implicit pointer scaling, so the element size has
-- to be applied here. Every site that moves a pointer goes through this: not
-- just @p + n@ but @p++@, @--p@ and @p += n@ too, which is why it is top-level
-- rather than local to 'genBinary'.
ptrStep :: Value -> Value -> Type -> String -> Type -> Lower Value
ptrStep pv iv el o resTy = do
  fn <- gets lsFn
  u64 <- u64L
  esz <- max 1 <$> sizeOf el
  scale <- lift (constInt fn u64 (fromIntegral esz))
  i <- lift (cast fn iv u64)
  off <- lift (binary fn "*" i scale u64)
  p <- lift (cast fn pv u64)
  sum' <- lift (binary fn o p off u64)
  t <- mtlcOf resTy
  lift (cast fn sum' t)

-- | The @+ 1@ / @- 1@ behind @++@ and @--@, scaled by the pointee size when the
-- operand is a pointer. @opTy@ is the operand's own type; @resTy@ types the
-- arithmetic in the ordinary scalar case.
stepOne :: Type -> Type -> Value -> String -> Lower Value
stepOne opTy resTy cur o = do
  fn <- gets lsFn
  case typeDecay opTy of
    TPtr el -> do
      u64 <- u64L
      one <- lift (constInt fn u64 1)
      ptrStep cur one el o opTy
    _ -> do
      t <- mtlcOf resTy
      one <- lift (constInt fn t 1)
      lift (binary fn o cur one t)

-- ---- memory ----

-- | Contiguous stack storage of @n@ bytes: a local of blob type, plus its
-- address.
stackBytes :: Int -> Maybe String -> Lower Value
stackBytes n mname = do
  fn <- gets lsFn
  bt <- lift (tyBlob (max 1 n))
  p <- i8pL
  nm <- maybe (freshTmp "stk") pure mname
  loc <- lift (local fn nm bt)
  lift (addressOf fn loc p)

-- | Heap storage: for VLAs, and for globals, whose lifetime outlives any frame.
heapBytes :: Either Int Value -> Lower Value
heapBytes what = do
  fn <- gets lsFn
  p <- i8pL
  u64 <- u64L
  n <- case what of
    Right v -> pure v
    Left b -> lift (constInt fn u64 (fromIntegral (max 1 b)))
  lift (call fn "malloc" [n] p)

-- | The widest scalar chunk that fits in @rem@ bytes, and its width.
chunkType :: Int -> Lower (Ty, Int)
chunkType rem'
  | rem' >= 8 = (,8) <$> u64L
  | rem' >= 4 = (,4) <$> lift (tyScalar U32)
  | rem' >= 2 = (,2) <$> lift (tyScalar U16)
  | otherwise = (,1) <$> lift (tyScalar U8)

-- | @base + off@, as a pointer to @ct@.
chunkAddr :: Value -> Int -> Ty -> Lower Value
chunkAddr base off ct = do
  fn <- gets lsFn
  u64 <- u64L
  p <- i8pL
  a <-
    if off == 0
      then pure base
      else do
        o <- lift (constInt fn u64 (fromIntegral off))
        lift (binary fn "+" base o p)
  pt <- lift (tyPointer ct)
  lift (cast fn a pt)

-- | Zero @n@ bytes (C99's aggregate default). Wide stores when small, memset
-- beyond.
memZero :: Value -> Int -> Lower ()
memZero addr n
  | n > 64 = do
      fn <- gets lsFn
      u64 <- u64L
      i32 <- i32L
      p <- i8pL
      z <- lift (constInt fn i32 0)
      cnt <- lift (constInt fn u64 (fromIntegral n))
      lift (callVoid fn "memset" [addr, z, cnt] p)
  | otherwise = go 0
  where
    go off
      | off >= n = pure ()
      | otherwise = do
          fn <- gets lsFn
          (ct, step) <- chunkType (n - off)
          a <- chunkAddr addr off ct
          z <- lift (constInt fn ct 0)
          lift (store fn a z ct)
          go (off + step)

memCopy :: Value -> Value -> Int -> Lower ()
memCopy dst src n
  | n > 64 = do
      fn <- gets lsFn
      u64 <- u64L
      p <- i8pL
      cnt <- lift (constInt fn u64 (fromIntegral n))
      lift (callVoid fn "memcpy" [dst, src, cnt] p)
  | otherwise = go 0
  where
    go off
      | off >= n = pure ()
      | otherwise = do
          fn <- gets lsFn
          (ct, step) <- chunkType (n - off)
          sa <- chunkAddr src off ct
          da <- chunkAddr dst off ct
          v <- lift (load fn sa ct)
          lift (store fn da v ct)
          go (off + step)

-- ---- externs ----

-- | Declare an extern function once, with a prototype-accurate signature.
-- A variadic gets its fixed parameters plus an i64 tail padded to
-- 'variadicPad', so every call site can agree on one signature.
ensureExternFn :: String -> Type -> Lower ()
ensureExternFn name ft = do
  declared <- gets lsDeclaredExterns
  unless (name `elem` declared) $ do
    modify' $ \s -> s {lsDeclaredExterns = name : lsDeclaredExterns s}
    b <- gets lsBuilder
    let (fixed, variadic, retT) = case ft of
          TFunc r ps var _ -> (ps, var, r)
          _ -> ([], False, TInt)
        sret = isAgg retT && not (isArray retT)
        nparams = max (length fixed) (if variadic then variadicPad else length fixed)
    i64 <- i64L
    p <- i8pL
    fixedTys <- mapM mtlcOf fixed
    let padTys = replicate (nparams - length fixed) i64
        ptys = fixedTys ++ padTys
    rt <-
      if sret
        then pure p
        else
          if retT == TVoid
            then lift (tyScalar Void)
            else mtlcOf retT
    lift (builderExtern b name rt (if sret then p : ptys else ptys))
  where
    isArray TArray {} = True
    isArray _ = False

-- | Reinterpret a float's bits as i64 through a stack slot: Win64 varargs read
-- doubles out of integer argument slots, so a numeric conversion would be wrong.
floatBitsAsI64 :: Value -> Lower Value
floatBitsAsI64 v = do
  fn <- gets lsFn
  f64 <- f64L
  i64 <- i64L
  d <- lift (cast fn v f64) -- default argument promotion: float -> double
  slot <- stackBytes 8 Nothing
  lift (store fn slot d f64)
  ip <- lift (tyPointer i64)
  a <- lift (cast fn slot ip)
  lift (load fn a i64)

-- ---- globals ----

-- | A global that needs a real address: an aggregate, or a scalar whose address
-- is taken. Its IR global is a pointer, filled in by the constructor.
isAddrGlobal :: Symbol -> Bool
isAddrGlobal sym =
  symIsGlobal sym && (symAddrTaken sym || isAgg (symType sym))

-- | How to reach an lvalue's storage.
--
-- A plain scalar global has no address to take: @mtlc_address_of@ is defined
-- only for locals and parameters, which is why an addressable global is turned
-- into a pointer global by the constructor in the first place. Such a global
-- has to be read and written through its own handle.
--
-- Assignment already knew this and went through the handle. The
-- read-modify-write sites did not: they asked 'genLvalueAddr' for an address,
-- got the result of an unsupported @addressOf@, and loaded 0 through it. So
-- @int g = 5; ++g@ yielded 1.
data LvalRef
  = -- | Read it as a value, write it with @assign@.
    LvalHandle Value
  | -- | Load and store through this address.
    LvalAddr Value

lvalRef :: Expr -> Lower LvalRef
lvalRef e = case exNode e of
  EIdent _ (Just sid) -> do
    sym <- symOf sid
    fn <- gets lsFn
    if symIsGlobal sym && not (isAddrGlobal sym)
      then LvalHandle <$> lift (globalRef fn (symLinkName sym))
      else LvalAddr <$> genLvalueAddr e
  _ -> LvalAddr <$> genLvalueAddr e

readLval :: LvalRef -> Type -> Lower Value
readLval (LvalHandle v) _ = pure v
readLval (LvalAddr a) t = do
  fn <- gets lsFn
  ty <- mtlcOf t
  lift (load fn a ty)

-- | Copy a value into a fresh temporary.
--
-- 'readLval' on a 'LvalHandle' hands back the storage itself, not a snapshot
-- of it, so a later write through the same handle changes what the earlier
-- read appears to say. @g++@ has to yield the value from before the increment,
-- which means copying it out first. A load through an address is already a
-- snapshot, but copying it too keeps the one rule instead of two.
materialize :: Value -> Type -> Lower Value
materialize v t = do
  fn <- gets lsFn
  ty <- mtlcOf t
  nm <- freshTmp "old"
  tmp <- lift (local fn nm ty)
  lift (assign fn tmp v)
  pure tmp

writeLval :: LvalRef -> Type -> Value -> Lower ()
writeLval (LvalHandle h) _ v = do
  fn <- gets lsFn
  lift (assign fn h v)
writeLval (LvalAddr a) t v = do
  fn <- gets lsFn
  ty <- mtlcOf t
  lift (store fn a v ty)

-- | Allocate and zero an addressable global's storage if it is still null, then
-- hand back the pointer global. Idempotent, so the constructor and any lazy use
-- can both call it.
ensureAggGlobal :: String -> Int -> Lower Value
ensureAggGlobal name bytes = do
  fn <- gets lsFn
  g <- lift (globalRef fn name)
  done <- freshLabel "gdone"
  need <- freshLabel "ginit"
  lift (branchIfZero fn g need)
  lift (jump fn done)
  lift (label fn need)
  mem <- heapBytes (Left (max 1 bytes)) -- a global outlives every frame
  memZero mem (max 1 bytes)
  lift (assign fn g mem)
  lift (label fn done)
  lift (globalRef fn name)

-- | Emit one file-scope initializer inside __c99m_init_globals.
applyGlobalInit :: Decl -> Lower ()
applyGlobalInit d = do
  sym <- mapM symOf (dSym d)
  let ty = maybe (dType d) symType sym
      name = maybe (dName d) symLinkName sym
      addrObj = isAgg ty || maybe False symAddrTaken sym
  fn <- gets lsFn
  if addrObj
    then do
      bytes <- sizeOf ty
      base <- ensureAggGlobal name (if bytes == 0 then 8 else bytes)
      case dInit d of
        Nothing -> pure ()
        Just ini
          | isAgg ty -> genInitInto ty base ini
          | otherwise -> do
              -- an address-taken scalar: store through the pointer
              v <- genInit1 ini
              ty' <- mtlcOf ty
              v' <- castTo v (initType ini) ty
              lift (store fn base v' ty')
    else case (ty, dInit d) of
      (TPtr _, Just ini) -> do
        g <- lift (globalRef fn name)
        v <- genInit1 ini
        lift (assign fn g v)
      _ -> pure ()
  where
    initType (IExpr e) = fromMaybe TInt (exTy e)
    initType _ = TInt

genInit1 :: Init -> Lower Value
genInit1 (IExpr e) = genExpr e
genInit1 (IList _) = do
  fn <- gets lsFn
  i32 <- i32L
  lift (constInt fn i32 0)

-- ---- string literals ----

-- | A string literal's address.
--
-- The content is packed into u64 data globals (so it lands in .data), and a
-- pointer global is copied to a heap buffer on first use. The public builder
-- cannot hand out a @char*@ into .data — mtlc_address_of is only defined for
-- locals and parameters — so this is the only way to get a stable pointer with
-- static duration.
genString :: String -> Lower Value
genString s = do
  interned <- gets lsStrings
  ptrname <- case M.lookup s interned of
    Just p -> pure p
    Nothing -> newString s
  ensureString ptrname s
  fn <- gets lsFn
  lift (globalRef fn ptrname)

newString :: String -> Lower String
newString s = do
  b <- gets lsBuilder
  n <- gets lsStrId
  modify' $ \st -> st {lsStrId = n + 1}
  let base = ".str" ++ show n
      bytes = map (fromIntegral . ord) s ++ [0] :: [Word64]
      words' = chunk8 bytes
  u64 <- u64L
  forM_ (zip [0 :: Int ..] words') $ \(w, packed) ->
    lift (builderGlobal b (base ++ "_" ++ show w) u64 (fromIntegral packed) False)
  p <- i8pL
  let ptrname = base ++ "_p"
  lift (builderGlobal b ptrname p 0 False)
  modify' $ \st -> st {lsStrings = M.insert s ptrname (lsStrings st)}
  pure ptrname
  where
    chunk8 [] = []
    chunk8 xs =
      let (h, t) = splitAt 8 xs
          packed = sum [b' `shiftL` (8 * i) | (i, b') <- zip [0 ..] h]
       in packed : chunk8 t

-- | Emit the first-use check for a string's pointer global, once per function.
ensureString :: String -> String -> Lower ()
ensureString ptrname s = do
  ensured <- gets lsFnStrEnsured
  unless (ptrname `elem` ensured) $ do
    modify' $ \st -> st {lsFnStrEnsured = ptrname : lsFnStrEnsured st}
    fn <- gets lsFn
    u64 <- u64L
    i8 <- lift (tyScalar I8)
    p <- i8pL
    g <- lift (globalRef fn ptrname)
    done <- freshLabel "sd"
    need <- freshLabel "si"
    lift (branchIfZero fn g need)
    lift (jump fn done)
    lift (label fn need)
    let bytes = map ord s ++ [0]
    mem <- heapBytes (Left (length bytes))
    forM_ (zip [0 :: Int ..] bytes) $ \(i, ch) -> do
      off <- lift (constInt fn u64 (fromIntegral i))
      a <- lift (binary fn "+" mem off p)
      c <- lift (constInt fn i8 (fromIntegral ch))
      lift (store fn a c i8)
    lift (assign fn g mem)
    lift (label fn done)

-- ---- lvalues ----

genLvalueAddr :: Expr -> Lower Value
genLvalueAddr e = case exNode e of
  EIdent name msid -> case msid of
    Nothing -> do
      err (exLoc e) ("no symbol for '" ++ name ++ "'")
      fn <- gets lsFn
      i32 <- i32L
      lift (constInt fn i32 0)
    Just sid -> do
      ptrs <- gets lsPtrs
      case M.lookup sid ptrs of
        Just p -> pure p -- an aggregate local: the handle IS the address
        Nothing -> do
          sym <- symOf sid
          fn <- gets lsFn
          if symIsGlobal sym
            then
              if isAddrGlobal sym
                then do
                  sz <- sizeOf (symType sym)
                  ensureAggGlobal (symLinkName sym) (if sz == 0 then 8 else sz)
                else do
                  g <- lift (globalRef fn (symLinkName sym))
                  ty <- mtlcOf (exprTy e)
                  pt <- lift (tyPointer ty)
                  lift (addressOf fn g pt)
            else do
              locals <- gets lsLocals
              let loc = locals M.! sid
              case exprTy e of
                TArray {} -> pure loc
                _ -> do
                  ty <- mtlcOf (exprTy e)
                  pt <- lift (tyPointer ty)
                  lift (addressOf fn loc pt)
  EUnary Deref x -> genExpr x
  EIndex base idx -> do
    fn <- gets lsFn
    b <- genExpr base
    i <- genExpr idx
    u64 <- u64L
    esz <- case typeDecay (exprTy base) of
      TPtr el -> max 1 <$> sizeOf el
      _ -> max 1 <$> sizeOf (exprTy e)
    scale <- lift (constInt fn u64 (fromIntegral esz))
    i' <- lift (cast fn i u64)
    off <- lift (binary fn "*" i' scale u64)
    b' <- lift (cast fn b u64)
    sum' <- lift (binary fn "+" b' off u64)
    ty <- mtlcOf (exprTy e)
    pt <- lift (tyPointer ty)
    lift (cast fn sum' pt)
  EMember obj name arrow -> do
    fn <- gets lsFn
    base <- if arrow then genExpr obj else genLvalueAddr obj
    moff <- memberOffset obj name arrow
    u64 <- u64L
    off <- lift (constInt fn u64 (fromIntegral moff))
    b' <- lift (cast fn base u64)
    sum' <- lift (binary fn "+" b' off u64)
    ty <- mtlcOf (exprTy e)
    pt <- lift (tyPointer ty)
    lift (cast fn sum' pt)
  _ -> do
    err (exLoc e) "not an lvalue"
    fn <- gets lsFn
    i32 <- i32L
    lift (constInt fn i32 0)

exprTy :: Expr -> Type
exprTy = fromMaybe TInt . exTy

-- | The struct type an @x.f@ / @p->f@ names its member in.
memberStruct :: Expr -> Bool -> Type
memberStruct obj arrow
  | arrow = case typeDecay (exprTy obj) of
      TPtr b -> b
      t -> t
  | otherwise = exprTy obj

memberOf :: Expr -> String -> Bool -> Lower (Maybe Member)
memberOf obj name arrow = do
  tc <- gets lsTc
  pure (findMember tc (memberStruct obj arrow) name)

memberOffset :: Expr -> String -> Bool -> Lower Int
memberOffset obj name arrow = maybe 0 memOffset <$> memberOf obj name arrow

-- ---- bit-fields ----

bfLoad :: Value -> Member -> Lower Value
bfLoad addr m = do
  fn <- gets lsFn
  u32 <- lift (tyScalar U32)
  let (bitOff, width) = fromMaybe (0, 32) (memBits m)
  word <- lift (load fn addr u32)
  sh <- lift (constInt fn u32 (fromIntegral bitOff))
  shifted <- lift (binary fn ">>" word sh u32)
  mv <- lift (constInt fn u32 (fromIntegral (bfMask width)))
  raw <- lift (binary fn "&" shifted mv u32)
  -- A bit-field declared with a signed type is a signed type (C99 6.7.2.1),
  -- so its top bit is a sign bit. Masking alone leaves the value zero-extended,
  -- and a 4-bit field holding -1 reads as 15.
  --
  -- Sign-extend with (v XOR s) - s, where s is the field's sign bit. The
  -- textbook way is to shift the field up to the top of the word and back down
  -- arithmetically, but a shift whose left operand is itself a shift currently
  -- lowers to a logical shift, so this uses only XOR and subtract.
  if width > 0 && width < 32 && not (typeIsUnsigned (memType m))
    then do
      i32 <- i32L
      sv <- lift (cast fn raw i32)
      s <- lift (constInt fn i32 (fromIntegral (bit (width - 1) :: Integer)))
      flipped <- lift (binary fn "^" sv s i32)
      lift (binary fn "-" flipped s i32)
    else pure raw

bfStore :: Value -> Member -> Value -> Lower ()
bfStore addr m val = do
  fn <- gets lsFn
  u32 <- lift (tyScalar U32)
  let (bitOff, width) = fromMaybe (0, 32) (memBits m)
      mask = bfMask width
  word <- lift (load fn addr u32)
  mv <- lift (constInt fn u32 (fromIntegral mask))
  v0 <- lift (cast fn val u32)
  v1 <- lift (binary fn "&" v0 mv u32)
  sh <- lift (constInt fn u32 (fromIntegral bitOff))
  shifted <- lift (binary fn "<<" v1 sh u32)
  let clearMask = complement (mask `shiftL` bitOff) :: Word32
  cm <- lift (constInt fn u32 (fromIntegral clearMask))
  cleared <- lift (binary fn "&" word cm u32)
  merged <- lift (binary fn "|" cleared shifted u32)
  lift (store fn addr merged u32)

bfMask :: Int -> Word32
bfMask w
  | w >= 32 = 0xffffffff
  | otherwise = (1 `shiftL` w) - 1

-- ---- _Complex, as {double re; double im} in memory ----

cplxAlloc :: Lower Value
cplxAlloc = stackBytes 16 Nothing

cplxStore :: Value -> Value -> Value -> Lower ()
cplxStore addr re im = do
  fn <- gets lsFn
  f64 <- f64L
  u64 <- u64L
  lift (store fn addr re f64)
  off <- lift (constInt fn u64 8)
  a <- lift (cast fn addr u64)
  ai <- lift (binary fn "+" a off u64)
  pf <- lift (tyPointer f64)
  ai' <- lift (cast fn ai pf)
  lift (store fn ai' im f64)

cplxLoad :: Value -> Lower (Value, Value)
cplxLoad addr = do
  fn <- gets lsFn
  f64 <- f64L
  u64 <- u64L
  re <- lift (load fn addr f64)
  off <- lift (constInt fn u64 8)
  a <- lift (cast fn addr u64)
  ai <- lift (binary fn "+" a off u64)
  pf <- lift (tyPointer f64)
  ai' <- lift (cast fn ai pf)
  im <- lift (load fn ai' f64)
  pure (re, im)

-- ---- short-circuit control flow ----

-- | Evaluate @e@ for truth, branching to one of two labels. && and || never
-- materialize a value here; that is what makes them short-circuit.
genBool :: Expr -> String -> String -> Lower ()
genBool e trueL falseL = case exNode e of
  EBinary LAnd l r -> do
    mid <- freshLabel "and"
    genBool l mid falseL
    fn <- gets lsFn
    lift (label fn mid)
    genBool r trueL falseL
  EBinary LOr l r -> do
    mid <- freshLabel "or"
    genBool l trueL mid
    fn <- gets lsFn
    lift (label fn mid)
    genBool r trueL falseL
  EUnary Not x -> genBool x falseL trueL
  _ -> do
    v <- genExpr e
    fn <- gets lsFn
    lift (branchIfZero fn v falseL)
    lift (jump fn trueL)

-- ---- initializers ----

-- | Write an initializer into storage at @baseAddr@.
genInitInto :: Type -> Value -> Init -> Lower ()
genInitInto ty baseAddr ini = case (ty, ini) of
  (TStruct tid, IList items) -> do
    tc <- gets lsTc
    let members = tagMembers (tagInfo tc tid)
    _ <- foldM (structItem tc members) 0 items
    pure ()
  (TArray el _ _, IList items) -> do
    _ <- foldM (arrayItem el) 0 items
    pure ()
  (TArray el _ _, IExpr s)
    | EString str <- exNode s
    , isCharLike el -> do
        n <- arrayLimit ty (length str + 1)
        fn <- gets lsFn
        i8 <- lift (tyScalar I8)
        u64 <- u64L
        forM_ [0 .. n - 1] $ \i -> do
          let ch = if i < length str then ord (str !! i) else 0
          off <- lift (constInt fn u64 (fromIntegral i))
          a <- lift (cast fn baseAddr u64)
          addr <- lift (binary fn "+" a off u64)
          pi8 <- lift (tyPointer i8)
          addr' <- lift (cast fn addr pi8)
          c <- lift (constInt fn i8 (fromIntegral ch))
          lift (store fn addr' c i8)
  (_, IExpr x)
    -- aggregate copy-initialization: `P y = x;` copies the object's bytes
    | isAgg ty
    , not (isComplex ty)
    , isAgg (exprTy x) -> do
        src <- genExpr x -- an aggregate rvalue is its address
        sz <- sizeOf ty
        memCopy baseAddr src (max 1 sz)
    | isComplex ty -> do
        v <- genExpr x
        fn <- gets lsFn
        f64 <- f64L
        if isComplex (exprTy x)
          then do
            (re, im) <- cplxLoad v
            cplxStore baseAddr re im
          else do
            zero <- lift (constFloat fn f64 0)
            cplxStore baseAddr v zero
    | otherwise -> do
        v <- genExpr x
        fn <- gets lsFn
        t <- mtlcOf ty
        v' <- castTo v (exprTy x) ty
        lift (store fn baseAddr v' t)
  _ -> pure ()
  where
    isComplex (TComplex _) = True
    isComplex _ = False
    isCharLike t = t `elem` [TChar, TSChar, TUChar]

    arrayLimit (TArray _ n _) want
      | n > 0 = pure (min want n)
      | otherwise = pure want
    arrayLimit _ want = pure want

    -- struct/union members: a designator names the member, otherwise position
    structItem tc members seq' (mdes, item) = do
      let m = case mdes of
            Just (DField f) -> findMember tc ty f
            _ -> if seq' < length members then Just (members !! seq') else Nothing
          seq'' = case mdes of
            Just (DField _) -> seq'
            _ -> seq' + 1
      case m of
        Nothing -> pure seq''
        Just mem -> do
          addr <- offsetAddr baseAddr (memOffset mem) (memType mem)
          case (memBits mem, item) of
            (Just _, _) -> do
              v <- genInit1 item
              bfStore addr mem v
            (_, IList _) | isAgg (memType mem) -> genInitInto (memType mem) addr item
            _ -> genInitInto (memType mem) addr item
          pure seq''

    -- array elements: a [k] designator resets the running index
    arrayItem el seq' (mdes, item) = do
      let idx = case mdes of
            Just (DIndex ix) | Just k <- foldConst ix -> fromIntegral k
            _ -> seq'
      esz <- sizeOf el
      addr <- offsetAddr baseAddr (idx * esz) el
      genInitInto el addr item
      pure (idx + 1)

-- | @base + off@ as a pointer to @ty@.
offsetAddr :: Value -> Int -> Type -> Lower Value
offsetAddr base off ty = do
  fn <- gets lsFn
  u64 <- u64L
  o <- lift (constInt fn u64 (fromIntegral off))
  a <- lift (cast fn base u64)
  sum' <- lift (binary fn "+" a o u64)
  t <- mtlcOf ty
  pt <- lift (tyPointer t)
  lift (cast fn sum' pt)

-- ---- expressions ----

genExpr :: Expr -> Lower Value
genExpr e = case exNode e of
  EInt n _ -> do
    fn <- gets lsFn
    t <- mtlcOf (exprTy e)
    lift (constInt fn t n)
  EChar n -> do
    fn <- gets lsFn
    t <- mtlcOf (exprTy e)
    lift (constInt fn t n)
  EFloat d _ -> do
    fn <- gets lsFn
    t <- mtlcOf (exprTy e)
    lift (constFloat fn t d)
  EString s -> genString s
  EIdent _ (Just sid) -> genIdent e sid
  EIdent name Nothing -> do
    err (exLoc e) ("unresolved identifier '" ++ name ++ "'")
    fn <- gets lsFn
    i32 <- i32L
    lift (constInt fn i32 0)
  EBuiltin name args mty -> genBuiltin e name args mty
  EBinary op l r -> genBinary e op l r
  EUnary op x -> genUnary e op x
  EPostfix op x -> do
    ref <- lvalRef x
    cur <- readLval ref (exprTy x)
    old <- materialize cur (exprTy x)
    nv <- stepOne (exprTy x) (exprTy e) cur (if op == PostInc then "+" else "-")
    writeLval ref (exprTy x) nv
    pure old -- the postfix value is the one from before the write
  EAssign op l r -> genAssign e op l r
  ECall f args _ -> genCall e f args
  EIndex base _ -> do
    addr <- genLvalueAddr e
    -- The ELEMENT type decides value-vs-address. exTy may have been decayed
    -- (e.g. when this is a call argument), which must not turn an aggregate
    -- element into an 8-byte load.
    let elem' = case typeDecay (exprTy base) of
          TPtr el -> el
          _ -> exprTy e
    if isAgg elem'
      then pure addr
      else do
        fn <- gets lsFn
        t <- mtlcOf (exprTy e)
        lift (load fn addr t)
  EMember obj name arrow -> do
    m <- memberOf obj name arrow
    addr <- genLvalueAddr e
    case m of
      Just mem | isJust (memBits mem) -> bfLoad addr mem
      _ -> do
        -- same decay hazard as EIndex: judge by the member's own type
        let mt = maybe (exprTy e) memType m
        if isAgg mt || isAgg (exprTy e)
          then pure addr
          else do
            fn <- gets lsFn
            t <- mtlcOf (exprTy e)
            lift (load fn addr t)
  ECast ty x -> do
    v <- genExpr x
    case ty of
      TComplex _ -> pure v
      _ -> do
        fn <- gets lsFn
        t <- mtlcOf ty
        lift (cast fn v t)
  ESizeofExpr _ -> zeroI32 -- sema replaced these with literals
  ESizeofType _ -> zeroI32
  ECond c l r -> do
    fn <- gets lsFn
    nm <- freshTmp "cond"
    t <-
      if isAgg (exprTy e)
        then lift (tyBlob 8)
        else mtlcOf (exprTy e)
    r' <- lift (local fn nm t)
    tl <- freshLabel "ct"
    fl <- freshLabel "cf"
    end <- freshLabel "ce"
    genBool c tl fl
    lift (label fn tl)
    lv <- genExpr l
    lift (assign fn r' lv)
    lift (jump fn end)
    lift (label fn fl)
    rv <- genExpr r
    lift (assign fn r' rv)
    lift (label fn end)
    pure r'
  EComma l r -> genExpr l >> genExpr r
  ECompoundLit ty ini -> do
    sz <- sizeOf ty
    mem <- stackBytes (if sz == 0 then 8 else sz) Nothing
    genInitInto ty mem ini
    pure mem
  where
    zeroI32 = do
      fn <- gets lsFn
      i32 <- i32L
      lift (constInt fn i32 0)

genIdent :: Expr -> SymId -> Lower Value
genIdent e sid = do
  sym <- symOf sid
  fn <- gets lsFn
  case symKind sym of
    SymFunc -> do
      when (symIsExtern sym) $ ensureExternFn (symLinkName sym) (symType sym)
      lift (functionAddress fn (symLinkName sym))
    _ -> do
      ptrs <- gets lsPtrs
      case M.lookup sid ptrs of
        -- An aggregate local's handle holds the object's base address. After
        -- array-to-pointer decay exTy is a pointer, but the value is still that
        -- address: loading through it would be a dereference.
        Just p -> pure p
        Nothing
          | symIsGlobal sym ->
              if isAddrGlobal sym
                then do
                  sz <- sizeOf (symType sym)
                  base <- ensureAggGlobal (symLinkName sym) (if sz == 0 then 8 else sz)
                  -- Decide by the SYMBOL's type, not the expression's: sema
                  -- decays `a` to int* in `a[0]`, but the storage is still an
                  -- array, and its base address is the value — loading through
                  -- it would dereference the first element as a pointer.
                  if isAgg (symType sym)
                    then pure base
                    else do
                      t <- mtlcOf (exprTy e)
                      lift (load fn base t) -- address-taken scalar
                else lift (globalRef fn (symLinkName sym))
          | otherwise -> do
              locals <- gets lsLocals
              case M.lookup sid locals of
                Just v -> pure v
                Nothing -> do
                  err (exLoc e) ("no storage for '" ++ symName sym ++ "'")
                  i32 <- i32L
                  lift (constInt fn i32 0)

genBuiltin :: Expr -> String -> [Expr] -> Maybe Type -> Lower Value
genBuiltin _e name args mty = case name of
  "__c99m_I" -> do
    fn <- gets lsFn
    f64 <- f64L
    addr <- cplxAlloc
    re <- lift (constFloat fn f64 0)
    im <- lift (constFloat fn f64 1)
    cplxStore addr re im
    pure addr
  "__builtin_va_start" -> do
    mva <- gets lsVaParam
    case (mva, args) of
      (Just va, (ap : _)) -> do
        fn <- gets lsFn
        apAddr <- genLvalueAddr ap
        p <- i8pL
        lift (store fn apAddr va p)
      _ -> pure ()
    noValue'
  "__builtin_va_end" -> noValue'
  "__builtin_va_arg" -> case args of
    (ap : _) -> do
      fn <- gets lsFn
      let ty = fromMaybe TInt mty
      apSlot <- genLvalueAddr ap
      p <- i8pL
      u64 <- u64L
      cur <- lift (load fn apSlot p)
      t <- mtlcOf ty
      val <- lift (load fn cur t)
      eight <- lift (constInt fn u64 8)
      nxt <- lift (binary fn "+" cur eight p)
      lift (store fn apSlot nxt p)
      pure val
    [] -> zero
  _
    | name `elem` ["__real__", "__imag__"] -> case args of
        (a : _) -> do
          addr <- genExpr a
          (re, im) <- cplxLoad addr
          pure (if name == "__real__" then re else im)
        [] -> zero
    | otherwise -> zero
  where
    zero = do
      fn <- gets lsFn
      i32 <- i32L
      lift (constInt fn i32 0)
    -- void builtins still have to yield something; nothing consumes it
    noValue' = zero

genUnary :: Expr -> UnOp -> Expr -> Lower Value
genUnary e op x = case op of
  AddrOf -> case exNode x of
    EIdent _ (Just sid) -> do
      sym <- symOf sid
      if symKind sym == SymFunc
        then do
          fn <- gets lsFn
          when (symIsExtern sym) $ ensureExternFn (symLinkName sym) (symType sym)
          lift (functionAddress fn (symLinkName sym))
        else genLvalueAddr x
    _ -> genLvalueAddr x
  Deref -> do
    p <- genExpr x
    if isAgg (exprTy e)
      then pure p
      else do
        fn <- gets lsFn
        t <- mtlcOf (exprTy e)
        lift (load fn p t)
  PreInc -> incDec "+"
  PreDec -> incDec "-"
  UPlus -> genExpr x
  Neg -> do
    v <- genExpr x
    fn <- gets lsFn
    t <- mtlcOf (exprTy e)
    lift (unary fn "-" v t)
  BNot -> do
    v <- genExpr x
    fn <- gets lsFn
    t <- mtlcOf (exprTy e)
    lift (unary fn "~" v t)
  Not -> do
    v <- genExpr x
    fn <- gets lsFn
    i32 <- i32L
    lift (unary fn "!" v i32)
  where
    incDec o = do
      ref <- lvalRef x
      cur <- readLval ref (exprTy x)
      nv <- stepOne (exprTy x) (exprTy e) cur o
      writeLval ref (exprTy x) nv
      pure nv

genBinary :: Expr -> BinOp -> Expr -> Expr -> Lower Value
genBinary e op l r
  | op == LAnd || op == LOr = do
      -- materialize the short-circuit result as 0 or 1
      fn <- gets lsFn
      i32 <- i32L
      nm <- freshTmp "sc"
      res <- lift (local fn nm i32)
      tl <- freshLabel "t"
      fl <- freshLabel "f"
      end <- freshLabel "end"
      genBool e tl fl
      lift (label fn tl)
      one <- lift (constInt fn i32 1)
      lift (assign fn res one)
      lift (jump fn end)
      lift (label fn fl)
      zero <- lift (constInt fn i32 0)
      lift (assign fn res zero)
      lift (label fn end)
      pure res
  | TComplex _ <- exprTy e = genComplexBinary e op l r
  | otherwise = do
      fn <- gets lsFn
      lv <- genExpr l
      rv <- genExpr r
      tc <- gets lsTc
      let lt = typeDecay (exprTy l)
          rt = typeDecay (exprTy r)
      case () of
        _
          | op == Add, TPtr el <- lt, typeIsInteger rt -> ptrOffset lv rv el "+"
          | op == Add, TPtr el <- rt, typeIsInteger lt -> ptrOffset rv lv el "+"
          | op == Sub, TPtr el <- lt, TPtr _ <- rt -> ptrDiff lv rv el
          | op == Sub, TPtr el <- lt, typeIsInteger rt -> ptrOffset lv rv el "-"
          | otherwise -> do
              -- libmtlc picks signed-vs-unsigned and int-vs-float comparison
              -- off the result type, so compare in the common operand type.
              --
              -- A shift is the exception. C99 6.5.7p3 promotes each operand on
              -- its own and takes the result type from the LEFT one, so the
              -- usual arithmetic conversions must not run: an unsigned count
              -- would otherwise make the whole thing unsigned and turn an
              -- arithmetic shift into a logical one.
              let isShift = op == Shl || op == Shr
                  ct
                    | isShift = typePromote lt
                    | isCmp op =
                        if typeIsArithmetic lt && typeIsArithmetic rt
                          then typeUsualArith tc lt rt
                          else TULLong -- pointer comparisons are unsigned
                    | typeIsArithmetic lt && typeIsArithmetic rt =
                        typeUsualArith tc lt rt
                    | otherwise = exprTy e
              lv' <- castTo lv lt ct
              -- the count keeps its own type; only its value matters
              rv' <- if isShift then pure rv else castTo rv rt ct
              ct' <- mtlcOf ct
              res <- lift (binary fn (binOpText op) lv' rv' ct')
              if isCmp op && typeIsFloat ct
                then do
                  i32 <- i32L
                  lift (cast fn res i32)
                else pure res
  where
    isCmp o = o `elem` [Eq, Ne, Lt, Le, Gt, Ge]

    ptrOffset pv iv el o = ptrStep pv iv el o (exprTy e)

    ptrDiff lv rv el = do
      fn <- gets lsFn
      i64 <- i64L
      a <- lift (cast fn lv i64)
      b <- lift (cast fn rv i64)
      d <- lift (binary fn "-" a b i64)
      esz <- max 1 <$> sizeOf el
      d' <-
        if esz > 1
          then do
            scale <- lift (constInt fn i64 (fromIntegral esz))
            lift (binary fn "/" d scale i64)
          else pure d
      castTo d' TLLong (exprTy e)

genComplexBinary :: Expr -> BinOp -> Expr -> Expr -> Lower Value
genComplexBinary _e op l r = do
  fn <- gets lsFn
  f64 <- f64L
  la <- genExpr l
  ra <- genExpr r
  (lr, li) <- partsOf la (exprTy l)
  (rr, ri) <- partsOf ra (exprTy r)
  out <- cplxAlloc
  case op of
    Add -> do
      re <- lift (binary fn "+" lr rr f64)
      im <- lift (binary fn "+" li ri f64)
      cplxStore out re im
    Sub -> do
      re <- lift (binary fn "-" lr rr f64)
      im <- lift (binary fn "-" li ri f64)
      cplxStore out re im
    Mul -> do
      -- (lr + li i)(rr + ri i) = lr*rr - li*ri + (lr*ri + li*rr) i
      ac <- lift (binary fn "*" lr rr f64)
      bd <- lift (binary fn "*" li ri f64)
      re <- lift (binary fn "-" ac bd f64)
      ad <- lift (binary fn "*" lr ri f64)
      bc <- lift (binary fn "*" li rr f64)
      im <- lift (binary fn "+" ad bc f64)
      cplxStore out re im
    _ -> cplxStore out lr li
  pure out
  where
    partsOf v t
      | TComplex _ <- t = cplxLoad v
      | otherwise = do
          fn <- gets lsFn
          f64 <- f64L
          zero <- lift (constFloat fn f64 0)
          pure (v, zero)

genAssign :: Expr -> AssignOp -> Expr -> Expr -> Lower Value
genAssign e op lhs rhs
  -- bit-field member
  | EMember obj name arrow <- exNode lhs = do
      m <- memberOf obj name arrow
      case m of
        Just mem | isJust (memBits mem) -> do
          fn <- gets lsFn
          addr <- genLvalueAddr lhs
          rv <- genExpr rhs
          rv' <- case op of
            Assign -> pure rv
            AssignOp bop -> do
              cur <- bfLoad addr mem
              t <- mtlcOf (exprTy e)
              lift (binary fn (binOpText bop) cur rv t)
          bfStore addr mem rv'
          pure rv'
        _ -> general
  | otherwise = general
  where
    general
      -- whole-aggregate assignment copies the object representation
      | Assign <- op
      , isAgg (exprTy lhs)
      , not (isArray (exprTy lhs)) = do
          dst <- genLvalueAddr lhs
          src <- genExpr rhs -- an aggregate rvalue is its address
          sz <- sizeOf (exprTy lhs)
          memCopy dst src (max 1 sz)
          pure dst
      | AssignOp bop <- op = do
          fn <- gets lsFn
          rv <- genExpr rhs
          ref <- lvalRef lhs
          cur <- readLval ref (exprTy lhs)
          nv <- case typeDecay (exprTy lhs) of
            -- p += n / p -= n move by n elements, not n bytes
            TPtr el | bop == Add || bop == Sub ->
              ptrStep cur rv el (binOpText bop) (exprTy lhs)
            _ -> do
              t <- mtlcOf (exprTy e)
              lift (binary fn (binOpText bop) cur rv t)
          writeLval ref (exprTy lhs) nv
          pure nv
      | otherwise = do
          fn <- gets lsFn
          rv <- genExpr rhs
          case exNode lhs of
            EIdent _ (Just sid) -> do
              sym <- symOf sid
              ptrs <- gets lsPtrs
              locals <- gets lsLocals
              case () of
                _
                  -- a scalar local: assign straight to its handle
                  | not (symIsGlobal sym)
                  , not (M.member sid ptrs)
                  , Just loc <- M.lookup sid locals -> do
                      rv' <- castTo rv (exprTy rhs) (exprTy lhs)
                      lift (assign fn loc rv')
                      pure loc
                  -- a plain scalar global: assign straight to the global
                  | symIsGlobal sym
                  , not (isAgg (exprTy lhs))
                  , not (symAddrTaken sym) -> do
                      g <- lift (globalRef fn (symLinkName sym))
                      rv' <- castTo rv (exprTy rhs) (exprTy lhs)
                      lift (assign fn g rv')
                      pure g
                  | otherwise -> viaAddress fn rv
            _ -> viaAddress fn rv

    viaAddress fn rv = do
      addr <- genLvalueAddr lhs
      case exprTy lhs of
        TComplex _ -> do
          (re, im) <- cplxLoad rv
          cplxStore addr re im
          pure addr
        _ -> do
          lt <- mtlcOf (exprTy lhs)
          rv' <- castTo rv (exprTy rhs) (exprTy lhs)
          lift (store fn addr rv' lt)
          pure rv'

    isArray TArray {} = True
    isArray _ = False

-- ---- calls ----

genCall :: Expr -> Expr -> [Expr] -> Lower Value
genCall e f args = do
  msym <- case exNode f of
    EIdent _ (Just sid) -> do
      sym <- symOf sid
      pure (if symKind sym == SymFunc then Just sym else Nothing)
    _ -> pure Nothing
  case msym of
    Just sym -> genDirectCall e sym args
    Nothing -> do
      -- An indirect call: a real code address in a value.
      --
      -- The callee's signature still decides the calling convention, so read
      -- it off the pointer's type rather than ignoring it. Getting the
      -- aggregate return wrong here was a segfault, not a wrong answer: the
      -- callee wrote its result through a hidden pointer argument that was
      -- never passed.
      fn <- gets lsFn
      fp <- genExpr f
      let ft = case typeDecay (exprTy f) of
            TPtr t@(TFunc {}) -> t
            t@(TFunc {}) -> t
            _ -> TFunc (exprTy e) [] False False
          (fixed, retT) = case ft of
            TFunc r ps _ _ -> (ps, r)
            _ -> ([], exprTy e)
          sret = isAgg retT && not (isArrayTy retT)
      argv <- forM (zip [0 ..] args) $ \(i, a) -> do
        v <- genExpr a
        if i < length fixed && not (isAgg (fixed !! i))
          then do
            t <- mtlcOf (fixed !! i)
            lift (cast fn v t)
          else pure v
      if sret
        then do
          sz <- sizeOf retT
          buf <- stackBytes (max 1 sz) Nothing
          memZero buf (max 1 sz)
          p <- i8pL
          _ <- lift (callIndirect fn fp (buf : argv) p)
          pure buf
        else do
          rt <- retTy (exprTy e)
          lift (callIndirect fn fp argv rt)

retTy :: Type -> Lower Ty
retTy TVoid = lift (tyScalar Void)
retTy t = mtlcOf t

-- | An array return type cannot happen in C, but a decayed one can reach here.
isArrayTy :: Type -> Bool
isArrayTy TArray {} = True
isArrayTy _ = False

genDirectCall :: Expr -> Symbol -> [Expr] -> Lower Value
genDirectCall e sym args = do
  fn <- gets lsFn
  let ft = symType sym
      cname = symLinkName sym
      (fixed, variadic, retT) = case ft of
        TFunc r ps var _ -> (ps, var, r)
        _ -> ([], False, TInt)
      sret = isAgg retT && not (isArray retT)
  when (symIsExtern sym) $ ensureExternFn cname ft
  case () of
    _
      | variadic && symIsExtern sym -> externVariadic fn cname fixed retT
      | variadic -> userVariadic fn cname fixed retT sret
      | sret -> sretCall fn cname retT
      | otherwise -> plainCall fn cname fixed
  where
    isArray TArray {} = True
    isArray _ = False
    n = length args

    -- Extern variadic: fixed args by prototype, tail as i64 padded to the
    -- canonical arity so every call site matches one signature.
    externVariadic fn cname fixed retT = do
      i64 <- i64L
      let total = max variadicPad n
      argv <- forM (zip [0 ..] args) $ \(i, a) -> do
        v <- genExpr a
        if i < length fixed
          then do
            t <- mtlcOf (fixed !! i)
            lift (cast fn v t)
          else
            if typeIsFloat (exprTy a)
              then floatBitsAsI64 v
              else lift (cast fn v i64)
      pad <- forM [n .. total - 1] $ \_ -> lift (constInt fn i64 0)
      rt <- retTy retT
      lift (call fn cname (argv ++ pad) rt)

    -- User-defined variadic: trailing args are packed into a buffer the callee
    -- walks with __builtin_va_arg.
    userVariadic fn cname fixed retT sret = do
      u64 <- u64L
      i64 <- i64L
      f64 <- f64L
      p <- i8pL
      let nfixed = length fixed
          extra = max 0 (n - nfixed)
      msret <-
        if sret
          then do
            sz <- sizeOf retT
            buf <- stackBytes (max 1 sz) Nothing
            memZero buf (max 1 sz)
            pure (Just buf)
          else pure Nothing
      fixedArgs <- mapM genExpr (take nfixed args)
      buf <- stackBytes (max 1 extra * 8) Nothing
      forM_ (zip [0 ..] (drop nfixed args)) $ \(i, a) -> do
        v <- genExpr a
        off <- lift (constInt fn u64 (fromIntegral (i * 8 :: Int)))
        addr <- lift (binary fn "+" buf off p)
        if typeIsFloat (exprTy a)
          then do
            -- default argument promotion: store the double's BITS, not a
            -- numeric conversion to integer
            d <- lift (cast fn v f64)
            pf <- lift (tyPointer f64)
            fa <- lift (cast fn addr pf)
            lift (store fn fa d f64)
          else do
            iv <- lift (cast fn v i64)
            lift (store fn addr iv i64)
      rt <- if sret then i8pL else retTy retT
      r <- lift (call fn cname (maybe [] pure msret ++ fixedArgs ++ [buf]) rt)
      pure (fromMaybe r msret)

    -- Aggregate return: a hidden first argument pointing at caller storage.
    sretCall fn cname retT = do
      sz <- sizeOf retT
      buf <- stackBytes (max 1 sz) Nothing
      memZero buf (max 1 sz)
      argv <- mapM genExpr args
      p <- i8pL
      _ <- lift (call fn cname (buf : argv) p)
      pure buf

    plainCall fn cname fixed = do
      argv <- forM (zip [0 ..] args) $ \(i, a) -> do
        v <- genExpr a
        if symIsExtern sym && i < length fixed
          then do
            t <- mtlcOf (fixed !! i)
            lift (cast fn v t)
          else pure v
      rt <- retTy (exprTy e)
      lift (call fn cname argv rt)

-- ---- statements ----

genStmt :: Stmt -> Lower ()
genStmt st = do
  -- emittedReturn means "control cannot fall off the end here". Any other
  -- statement reopens the fallthrough path, so reset it: without this, a void
  -- function with an early `return;` falls off its end into the next
  -- function's code.
  case stNode st of
    SReturn _ -> pure ()
    SCompound _ -> pure ()
    SCase _ _ -> pure ()
    SDefault _ -> pure ()
    _ -> modify' (\s -> s {lsEmittedReturn = False})
  go (stNode st)
  -- A structured statement with internal labels reopens the fallthrough path
  -- even if a nested return ran.
  case stNode st of
    SIf {} -> reopen
    SWhile {} -> reopen
    SDo {} -> reopen
    SFor {} -> reopen
    SSwitch {} -> reopen
    _ -> pure ()
  where
    reopen = modify' (\s -> s {lsEmittedReturn = False})

    go n = case n of
      SNull -> pure ()
      SExpr x -> () <$ genExpr x
      SCompound items -> mapM_ genBlockItem items
      SIf c b me -> do
        fn <- gets lsFn
        tl <- freshLabel "then"
        fl <- freshLabel "else"
        end <- freshLabel "endif"
        genBool c tl fl
        lift (label fn tl)
        genStmt b
        lift (jump fn end)
        lift (label fn fl)
        mapM_ genStmt me
        lift (label fn end)
      SWhile c b -> do
        fn <- gets lsFn
        top <- freshLabel "wtop"
        body <- freshLabel "wbody"
        end <- freshLabel "wend"
        lift (label fn top)
        genBool c body end
        lift (label fn body)
        withLoop end top (genStmt b)
        lift (jump fn top)
        lift (label fn end)
      SDo b c -> do
        fn <- gets lsFn
        body <- freshLabel "dbody"
        cond <- freshLabel "dcond"
        end <- freshLabel "dend"
        lift (label fn body)
        withLoop end cond (genStmt b)
        lift (label fn cond)
        genBool c body end
        lift (label fn end)
      SFor i c inc b -> do
        fn <- gets lsFn
        top <- freshLabel "ftop"
        body <- freshLabel "fbody"
        incL <- freshLabel "finc"
        end <- freshLabel "fend"
        mapM_ genBlockItem i
        lift (label fn top)
        case c of
          Just cx -> genBool cx body end
          Nothing -> lift (jump fn body)
        lift (label fn body)
        withLoop end incL (genStmt b)
        lift (label fn incL)
        mapM_ (\x -> () <$ genExpr x) inc
        lift (jump fn top)
        lift (label fn end)
      SBreak -> do
        loops <- gets lsLoop
        fn <- gets lsFn
        case loops of
          ((brk, _) : _) -> lift (jump fn brk)
          [] -> pure ()
      SContinue -> do
        loops <- gets lsLoop
        fn <- gets lsFn
        case loops of
          ((_, cont) : _) -> lift (jump fn cont)
          [] -> pure ()
      SReturn mx -> do
        fn <- gets lsFn
        rt <- gets lsRetType
        msret <- gets lsSretParam
        case mx of
          Nothing -> lift (retVoid fn)
          Just x -> case (msret, rt) of
            (Just sret, Just t) | isAgg t -> do
              src <- genExpr x
              sz <- sizeOf t
              memCopy sret src (max 1 sz)
              lift (ret fn sret)
            _ -> do
              v <- genExpr x
              v' <- case rt of
                Just t | not (isAgg t) -> castTo v (exprTy x) t
                _ -> pure v
              lift (ret fn v')
        modify' (\s -> s {lsEmittedReturn = True})
      SSwitch c b -> genSwitch c b
      SCase v b -> do
        emitCaseLabel (fromMaybe 0 (foldConst v))
        genStmt b
      SDefault b -> do
        emitDefaultLabel
        genStmt b
      SGoto l -> do
        fn <- gets lsFn
        lift (jump fn (".G" ++ l))
      SLabel l b -> do
        fn <- gets lsFn
        lift (label fn (".G" ++ l))
        genStmt b

    withLoop brk cont act = do
      modify' (\s -> s {lsLoop = (brk, cont) : lsLoop s})
      _ <- act
      modify' (\s -> s {lsLoop = drop 1 (lsLoop s)})

genBlockItem :: BlockItem -> Lower ()
genBlockItem (BIStmt s) = genStmt s
genBlockItem (BIDecl ds) = mapM_ genVarDecl ds

-- | A switch: compare-and-jump dispatch, then the body with fall-through.
--
-- Labels are collected from anywhere in the body and emitted where they occur,
-- so a @case@ inside a nested block, an @if@ or a loop is reachable. Collecting
-- only the top level of the body made those silently unreachable, which is what
-- broke Duff's device: its labels live inside a @do@/@while@.
genSwitch :: Expr -> Stmt -> Lower ()
genSwitch cond body = do
  fn <- gets lsFn
  cv <- genExpr cond
  end <- freshLabel "swend"
  i32 <- i32L

  let (values, hasDefault) = switchLabelsOf body
  named <- forM values $ \v -> (,) v <$> freshLabel "case"
  defLabel <- if hasDefault then Just <$> freshLabel "default" else pure Nothing

  -- dispatch
  forM_ named $ \(v, lbl) -> do
    k <- lift (constInt fn i32 v)
    eq <- lift (binary fn "==" cv k i32)
    next <- freshLabel "swn"
    lift (branchIfZero fn eq next)
    lift (jump fn lbl)
    lift (label fn next)
  lift (jump fn (fromMaybe end defLabel))

  -- body: SCase and SDefault emit their own labels from this table
  modify' $ \s ->
    s
      { lsLoop = (end, contOf s) : lsLoop s
      , lsSwitch = (M.fromList named, defLabel) : lsSwitch s
      }
  genStmt body
  modify' $ \s -> s {lsLoop = drop 1 (lsLoop s), lsSwitch = drop 1 (lsSwitch s)}
  lift (label fn end)
  where
    -- a switch is a break target but not a continue target: continue keeps
    -- belonging to any enclosing loop
    contOf s = case lsLoop s of
      ((_, c) : _) -> c
      [] -> ".Lswend_unreachable"

-- | Emit the label for a case value, from the innermost open switch.
emitCaseLabel :: Integer -> Lower ()
emitCaseLabel v = do
  st <- gets lsSwitch
  fn <- gets lsFn
  case st of
    ((tbl, _) : _) | Just lbl <- M.lookup v tbl -> lift (label fn lbl)
    _ -> pure () -- a case outside any switch; sema already reported it

emitDefaultLabel :: Lower ()
emitDefaultLabel = do
  st <- gets lsSwitch
  fn <- gets lsFn
  case st of
    ((_, Just lbl) : _) -> lift (label fn lbl)
    _ -> pure ()

-- | Every case value in this switch, and whether it has a default.
--
-- Does not descend into a nested switch, whose labels belong to it.
switchLabelsOf :: Stmt -> ([Integer], Bool)
switchLabelsOf = go
  where
    go s = case stNode s of
      SCase v b -> let (vs, d) = go b in (fromMaybe 0 (foldConst v) : vs, d)
      SDefault b -> let (vs, _) = go b in (vs, True)
      SSwitch {} -> ([], False)
      SCompound items -> mconcatPairs (map item items)
      SIf _ a mb -> mconcatPairs (go a : maybe [] ((: []) . go) mb)
      SWhile _ b -> go b
      SDo b _ -> go b
      SFor _ _ _ b -> go b
      SLabel _ b -> go b
      _ -> ([], False)

    item (BIStmt x) = go x
    item (BIDecl _) = ([], False)

    mconcatPairs ps = (concatMap fst ps, or (map snd ps))

-- ---- local declarations ----

genVarDecl :: Decl -> Lower ()
genVarDecl d = case dSym d of
  Nothing -> pure ()
  Just sid -> do
    sym <- symOf sid
    -- A block-scope static is a global; it was emitted with the other globals.
    if symIsGlobal sym
      then pure ()
      else do
        fn <- gets lsFn
        let ty = dType d
        if isAgg ty
          then do
            bytes <- sizeOf ty
            mem <- case ty of
              TArray el _ True -> do
                -- a VLA: size known only at run time, so it goes on the heap
                u64 <- u64L
                esz <- max 1 <$> sizeOf el
                cnt <- case dVlaSize d of
                  Just sz -> genExpr sz >>= \v -> lift (cast fn v u64)
                  Nothing -> lift (constInt fn u64 1)
                es <- lift (constInt fn u64 (fromIntegral esz))
                nb <- lift (binary fn "*" cnt es u64)
                heapBytes (Right nb)
              _ -> do
                mem <- stackBytes (max 1 bytes) (Just (dName d))
                memZero mem (max 1 bytes)
                pure mem
            -- a pointer local holding the object's base, for indexing and decay
            p <- i8pL
            loc <- lift (local fn (dName d ++ "_p") p)
            lift (assign fn loc mem)
            modify' $ \s ->
              s
                { lsLocals = M.insert sid loc (lsLocals s)
                , lsPtrs = M.insert sid loc (lsPtrs s)
                }
            case dInit d of
              Nothing -> pure ()
              Just ini
                -- initialized from a call returning an aggregate: hand the
                -- callee our storage as its sret argument, no copy
                | IExpr callE <- ini
                , ECall f cargs _ <- exNode callE
                , not (isArrayTy ty)
                , EIdent _ (Just fsid) <- exNode f -> do
                    fsym <- symOf fsid
                    if symKind fsym == SymFunc
                      then do
                        argv <- mapM genExpr cargs
                        p8 <- i8pL
                        _ <- lift (call fn (symLinkName fsym) (loc : argv) p8)
                        pure ()
                      else genInitInto ty loc ini
                | otherwise -> genInitInto ty loc ini
          else do
            t <- mtlcOf ty
            loc <- lift (local fn (dName d) t)
            modify' $ \s -> s {lsLocals = M.insert sid loc (lsLocals s)}
            case dInit d of
              Nothing -> pure ()
              Just (IExpr x) -> do
                v <- genExpr x
                v' <- castTo v (exprTy x) ty
                lift (assign fn loc v')
              Just ini -> genInitInto ty loc ini

-- ---- functions ----

genFunction :: FuncDef -> Lower ()
genFunction fd = do
  b <- gets lsBuilder
  let ft = fdType fd
      retT = funcRet ft
      params = fdParams fd
      isVar = fdVariadic fd
      isSret = isAgg retT && not (isArrayTy retT)
  p <- i8pL
  ptys <- mapM (mtlcOf . pType) params
  let pnames = zipWith pname [0 :: Int ..] params
      pname i prm = if null (pName prm) then "arg" ++ show i else pName prm
      irParams =
        [("__sret", p) | isSret]
          ++ zip pnames ptys
          ++ [("__va", p) | isVar]
  retMtlc <- if isSret then pure p else retTy retT
  mfn <- lift (builderFunction b (fdName fd) retMtlc irParams)
  case mfn of
    Nothing -> err (fdLoc fd) ("failed to create function '" ++ fdName fd ++ "'")
    Just fn -> do
      modify' $ \s ->
        s
          { lsFn = fn
          , lsLocals = M.empty
          , lsPtrs = M.empty
          , lsFnStrEnsured = []
          , lsLoop = []
          , lsRetType = Just retT
          , lsEmittedReturn = False
          , lsVaParam = Nothing
          , lsSretParam = Nothing
          }
      let arg0 = if isSret then 1 else 0
      when isSret $ do
        v <- lift (fnParam fn 0)
        modify' (\s -> s {lsSretParam = Just v})
      forM_ (zip [0 ..] params) $ \(i, prm) -> do
        v <- lift (fnParam fn (arg0 + i))
        case pSym prm of
          Nothing -> pure ()
          Just sid -> do
            let pt = pType prm
            if isAgg pt && not (isArrayTy pt)
              then do
                -- Passed by value: the incoming IR value points at the CALLER's
                -- object, so copy it into our own storage for C semantics.
                sz <- sizeOf pt
                let nm = if null (pName prm) then "arg" else pName prm
                copy <- stackBytes (max 1 sz) (Just (nm ++ "_byval"))
                memCopy copy v (max 1 sz)
                loc <- lift (local fn (nm ++ "_p") p)
                lift (assign fn loc copy)
                modify' $ \s ->
                  s
                    { lsLocals = M.insert sid loc (lsLocals s)
                    , lsPtrs = M.insert sid loc (lsPtrs s)
                    }
              else modify' (\s -> s {lsLocals = M.insert sid v (lsLocals s)})
      when isVar $ do
        v <- lift (fnParam fn (arg0 + length params))
        modify' (\s -> s {lsVaParam = Just v})

      -- file-scope initializers run before user main
      needInit <- gets lsNeedGlobalInit
      when (needInit && fdName fd == "main") $ do
        void' <- lift (tyScalar Void)
        lift (callVoid fn "__c99m_init_globals" [] void')

      genStmt (fdBody fd)

      emitted <- gets lsEmittedReturn
      unless emitted $ do
        msret <- gets lsSretParam
        case msret of
          Just sret -> lift (ret fn sret)
          Nothing
            | retT == TVoid -> lift (retVoid fn)
            | otherwise -> do
                t <- mtlcOf retT
                z <- lift (constInt fn t 0)
                lift (ret fn z)

-- ---- globals and the module ----

-- | Emit a file-scope variable. Aggregates and address-taken scalars become
-- pointer globals filled in by the constructor; a pointer global with a
-- non-integer initializer also needs the constructor.
-- | Every object needing a module-level global, each listed once.
--
-- A name declared @extern@ in a header and defined in one unit reaches here as
-- several declarations of the same symbol; only the defining one — the one with
-- the initializer — may be emitted, or the definition would be re-declared as
-- an extern and the symbol would go unresolved at link time. Block-scope
-- statics are included: they have static duration, so they are globals too.
globalVarsOf :: Program -> [Decl]
globalVarsOf prog = dedupe [] M.empty candidates
  where
    candidates =
      [d | TDDecl d <- prog, dStorage d /= ScTypedef]
        ++ concat [staticsIn (fdBody fd) | TDFunc fd <- prog]

    -- keep first-seen order, but let a declaration with an initializer replace
    -- an earlier one without
    dedupe out _ [] = reverse out
    dedupe out seen (d : ds) = case dSym d of
      Nothing -> dedupe out seen ds
      Just sid -> case M.lookup sid seen of
        Nothing -> dedupe (d : out) (M.insert sid d seen) ds
        Just prev
          | isJust (dInit d) && not (isJust (dInit prev)) ->
              dedupe (map (replace sid d) out) (M.insert sid d seen) ds
          | otherwise -> dedupe out seen ds

    replace sid new old
      | dSym old == Just sid = new
      | otherwise = old

declareVar :: Decl -> Lower ()
declareVar d
  | Just sid <- dSym d = do
      sym <- symOf sid
      case symKind sym of
        SymVar -> do
          b <- gets lsBuilder
          -- the symbol's type, not the declaration's: an `extern int t[];` in a
          -- header has no bound, but the definition does
          let ty = symType sym
              name = symLinkName sym
              isExtern = symIsExtern sym
              addrObj = isAgg ty || symAddrTaken sym
          if addrObj
            then do
              p <- i8pL
              sz <- sizeOf ty
              lift (builderGlobal b name p 0 isExtern)
              modify' $ \s ->
                s
                  { lsAggGlobals = lsAggGlobals s ++ [(name, if sz == 0 then 8 else sz)]
                  , lsGlobalInits = lsGlobalInits s ++ [d]
                  }
            else case (ty, dInit d) of
              (TPtr _, Just ini)
                | not (isIntLit ini) -> do
                    -- a string, &obj, or a cast: needs code to materialize
                    t <- mtlcOf ty
                    lift (builderGlobal b name t 0 isExtern)
                    modify' $ \s -> s {lsGlobalInits = lsGlobalInits s ++ [d]}
              _ -> do
                t <- mtlcOf ty
                lift (builderGlobal b name t (initConst (dInit d)) isExtern)
        _ -> pure ()
  where
    isIntLit (IExpr x) | EInt _ _ <- exNode x = True
    isIntLit _ = False
    initConst (Just (IExpr x)) | EInt n _ <- exNode x = n
    initConst _ = 0
declareVar _ = pure ()

-- | Every @static@ declared inside a function body, at any depth.
staticsIn :: Stmt -> [Decl]
staticsIn st = case stNode st of
  SCompound items -> concatMap item items
  SIf _ b me -> staticsIn b ++ concatMap staticsIn me
  SWhile _ b -> staticsIn b
  SDo b _ -> staticsIn b
  SFor i _ _ b -> maybe [] item i ++ staticsIn b
  SSwitch _ b -> staticsIn b
  SCase _ b -> staticsIn b
  SDefault b -> staticsIn b
  SLabel _ b -> staticsIn b
  _ -> []
  where
    item (BIStmt s) = staticsIn s
    item (BIDecl ds) = filter ((== ScStatic) . dStorage) ds

-- | The constructor: allocate every addressable global, then apply the
-- file-scope initializers in declaration order. main calls it first.
emitGlobalCtor :: Lower ()
emitGlobalCtor = do
  inits <- gets lsGlobalInits
  unless (null inits) $ do
    b <- gets lsBuilder
    void' <- lift (tyScalar Void)
    mfn <- lift (builderFunction b "__c99m_init_globals" void' [])
    case mfn of
      Nothing -> err noLoc "failed to create __c99m_init_globals"
      Just fn -> do
        modify' $ \s ->
          s
            { lsFn = fn
            , lsNeedGlobalInit = True
            , lsLocals = M.empty
            , lsPtrs = M.empty
            , lsFnStrEnsured = []
            , lsLoop = []
            , lsRetType = Nothing
            , lsEmittedReturn = False
            , lsVaParam = Nothing
            , lsSretParam = Nothing
            }
        aggs <- gets lsAggGlobals
        forM_ aggs $ \(name, sz) -> ensureAggGlobal name sz
        forM_ inits applyGlobalInit
        lift (retVoid fn)

-- | The C runtime entry points the lowerer itself emits calls to.
declareRuntime :: Builder -> IO ()
declareRuntime b = do
  i32 <- tyScalar I32
  u64 <- tyScalar U64
  v <- tyScalar Void
  p <- i8p
  builderExtern b "malloc" p [u64]
  builderExtern b "free" v [p]
  builderExtern b "putchar" i32 [i32]
  builderExtern b "getchar" i32 []
  builderExtern b "exit" v [i32]
  builderExtern b "memcpy" p [p, p, u64]
  builderExtern b "memset" p [p, i32, u64]
