-- | A Haskell-shaped interface to libmtlc.
--
-- Wraps "Mtlc.FFI": Strings instead of CStrings, lists instead of
-- pointer+count pairs, Maybe instead of the MTLC_NO_VALUE sentinel. The
-- lowering pass (see "C99.Lower") should not need to touch Foreign.* at all.
module Mtlc
  ( -- * Handles
    Builder
  , Fn
  , Module
  , Context
  , Ty
  , Value (..)
    -- * Types
  , TyKind (..)
  , tyScalar
  , tyPointer
  , tyBlob
    -- * Builder
  , builderCreate
  , builderDestroy
  , builderFinish
  , builderFunction
  , builderExtern
  , builderGlobal
  , moduleFunctionCount
  , moduleDestroy
    -- * Values
  , fnParam
  , constInt
  , constFloat
  , local
  , globalRef
    -- * Instructions
  , assign
  , binary
  , unary
  , call
  , callVoid
  , functionAddress
  , callIndirect
  , cast
  , addressOf
  , load
  , store
    -- * Control flow
  , label
  , jump
  , branchIfZero
  , ret
  , retVoid
    -- * Pipeline
  , contextCreate
  , contextDestroy
  , contextSetOptLevel
  , contextSetWholeProgram
  , optimize
  , emitObject
  , buildExecutable
  , version
  ) where

import Control.Monad (void)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Array (withArray, withArrayLen)
import Foreign.Marshal.Utils (fromBool)
import Foreign.Ptr (Ptr, nullPtr)

import qualified Mtlc.FFI as F

type Builder = F.BuilderPtr
type Fn = F.FnPtr'
type Module = F.ModulePtr
type Context = F.ContextPtr
type Ty = F.TypePtr

-- | An IR value handle. Scoped to the function that produced it.
newtype Value = Value F.MtlcValue
  deriving (Eq, Show)

-- | The subset of MtlcTypeKind a C frontend produces. Aggregates reach the
-- backend as byte arrays (see 'C99.Lower.mtlcBlob'), never as MTLC_TYPE_STRUCT,
-- which is why STRUCT and friends are absent here.
data TyKind
  = I8 | I16 | I32 | I64
  | U8 | U16 | U32 | U64
  | Bool
  | F32 | F64
  | Void
  deriving (Eq, Show)

kindCode :: TyKind -> CInt
kindCode k = case k of
  I8 -> F.mtlcInt8
  I16 -> F.mtlcInt16
  I32 -> F.mtlcInt32
  I64 -> F.mtlcInt64
  U8 -> F.mtlcUint8
  U16 -> F.mtlcUint16
  U32 -> F.mtlcUint32
  U64 -> F.mtlcUint64
  Bool -> F.mtlcBool
  F32 -> F.mtlcFloat32
  F64 -> F.mtlcFloat64
  Void -> F.mtlcVoid

-- | Canonical scalar descriptor. Immortal; never freed.
tyScalar :: TyKind -> IO Ty
tyScalar = F.c_mtlc_type_scalar . kindCode

-- | Canonical pointer-to-base descriptor. Interned by the backend.
tyPointer :: Ty -> IO Ty
tyPointer = F.c_mtlc_type_pointer

-- | An array-of-uint8 descriptor of @n@ bytes (rounded up to 8), for giving a
-- C aggregate contiguous stack storage via 'local'.
tyBlob :: Int -> IO Ty
tyBlob n = F.c_c99m_blob_type (fromIntegral n)

builderCreate :: IO Builder
builderCreate = F.c_mtlc_builder_create

builderDestroy :: Builder -> IO ()
builderDestroy = F.c_mtlc_builder_destroy

-- | Consumes the builder. Returns Nothing on backend error.
builderFinish :: Builder -> IO (Maybe Module)
builderFinish b = nonNull <$> F.c_mtlc_builder_finish b

-- | Declare a function with a body to emit into.
builderFunction :: Builder -> String -> Ty -> [(String, Ty)] -> IO (Maybe Fn)
builderFunction b name retTy params =
  withCString name $ \cname ->
    withCStrings (map fst params) $ \cnames ->
      withArrayLen (map snd params) $ \n ctypes ->
        withArray cnames $ \cnamesArr ->
          nonNull
            <$> F.c_mtlc_builder_function
                  b cname retTy cnamesArr ctypes (fromIntegral n) 0

-- | Declare a body-less external symbol. There is no Fn to emit into.
builderExtern :: Builder -> String -> Ty -> [Ty] -> IO ()
builderExtern b name retTy ptys =
  withCString name $ \cname ->
    withCStrings (replicate (length ptys) "") $ \cnames ->
      withArrayLen ptys $ \n ctypes ->
        withArray cnames $ \cnamesArr ->
          void $
            F.c_mtlc_builder_function
              b cname retTy cnamesArr ctypes (fromIntegral n) 1

-- | A module-level global. @init@ is a constant integer initializer; aggregate
-- and non-constant initializers are emitted as stores in a startup function by
-- the lowerer, since the backend's global initializer is int-only.
builderGlobal :: Builder -> String -> Ty -> Integer -> Bool -> IO ()
builderGlobal b name ty initVal isExtern =
  withCString name $ \cname ->
    F.c_mtlc_builder_global
      b cname ty (fromIntegral initVal) (fromBool isExtern)

moduleFunctionCount :: Module -> IO Int
moduleFunctionCount m = fromIntegral <$> F.c_mtlc_module_function_count m

moduleDestroy :: Module -> IO ()
moduleDestroy = F.c_mtlc_module_destroy

fnParam :: Fn -> Int -> IO Value
fnParam fn i = Value <$> F.c_mtlc_fn_param fn (fromIntegral i)

constInt :: Fn -> Ty -> Integer -> IO Value
constInt fn ty n = Value <$> F.c_mtlc_const_int fn ty (fromIntegral n)

constFloat :: Fn -> Ty -> Double -> IO Value
constFloat fn ty x = Value <$> F.c_mtlc_const_float fn ty (realToFrac x)

-- | Declare a mutable local; the handle both reads and (via 'assign') writes.
local :: Fn -> String -> Ty -> IO Value
local fn name ty = withCString name $ \c -> Value <$> F.c_mtlc_local fn c ty

globalRef :: Fn -> String -> IO Value
globalRef fn name = withCString name $ \c -> Value <$> F.c_mtlc_global_ref fn c

assign :: Fn -> Value -> Value -> IO ()
assign fn (Value d) (Value v) = F.c_mtlc_assign fn d v

-- | @op@ is one of + - * / % == != < <= > >= && || & | ^ << >>.
binary :: Fn -> String -> Value -> Value -> Ty -> IO Value
binary fn op (Value l) (Value r) ty =
  withCString op $ \cop -> Value <$> F.c_mtlc_binary fn cop l r ty

-- | @op@ is one of - ! ~.
unary :: Fn -> String -> Value -> Ty -> IO Value
unary fn op (Value v) ty =
  withCString op $ \cop -> Value <$> F.c_mtlc_unary fn cop v ty

call :: Fn -> String -> [Value] -> Ty -> IO Value
call fn callee args ty =
  withCString callee $ \c ->
    withArrayLen (unwrap args) $ \n arr ->
      Value <$> F.c_mtlc_call fn c arr (fromIntegral n) ty

callVoid :: Fn -> String -> [Value] -> Ty -> IO ()
callVoid fn callee args ty = void $ call fn callee args ty

-- | The real address of a function symbol: a callback usable by C APIs.
functionAddress :: Fn -> String -> IO Value
functionAddress fn name =
  withCString name $ \c -> Value <$> F.c_mtlc_function_address fn c

callIndirect :: Fn -> Value -> [Value] -> Ty -> IO Value
callIndirect fn (Value callee) args ty =
  withArrayLen (unwrap args) $ \n arr ->
    Value <$> F.c_mtlc_call_indirect fn callee arr (fromIntegral n) ty

cast :: Fn -> Value -> Ty -> IO Value
cast fn (Value v) ty = Value <$> F.c_mtlc_cast fn v ty

addressOf :: Fn -> Value -> Ty -> IO Value
addressOf fn (Value storage) ptrTy =
  Value <$> F.c_mtlc_address_of fn storage ptrTy

load :: Fn -> Value -> Ty -> IO Value
load fn (Value addr) elemTy = Value <$> F.c_mtlc_load fn addr elemTy

store :: Fn -> Value -> Value -> Ty -> IO ()
store fn (Value addr) (Value v) elemTy = F.c_mtlc_store fn addr v elemTy

label :: Fn -> String -> IO ()
label fn l = withCString l (F.c_mtlc_label fn)

jump :: Fn -> String -> IO ()
jump fn l = withCString l (F.c_mtlc_jump fn)

branchIfZero :: Fn -> Value -> String -> IO ()
branchIfZero fn (Value c) l = withCString l (F.c_mtlc_branch_if_zero fn c)

ret :: Fn -> Value -> IO ()
ret fn (Value v) = F.c_mtlc_return fn v

retVoid :: Fn -> IO ()
retVoid fn = F.c_mtlc_return fn F.noValue

contextCreate :: IO Context
contextCreate = F.c_mtlc_context_create

contextDestroy :: Context -> IO ()
contextDestroy = F.c_mtlc_context_destroy

contextSetOptLevel :: Context -> Int -> IO ()
contextSetOptLevel ctx n = F.c_mtlc_context_set_opt_level ctx (fromIntegral n)

contextSetWholeProgram :: Context -> Bool -> IO ()
contextSetWholeProgram ctx b =
  F.c_mtlc_context_set_whole_program ctx (fromBool b)

optimize :: Context -> Module -> IO Bool
optimize ctx m = toBool <$> F.c_mtlc_optimize ctx m

emitObject :: Context -> Module -> FilePath -> IO Bool
emitObject ctx m path =
  withCString path $ \c -> toBool <$> F.c_mtlc_emit_object ctx m c

buildExecutable :: Context -> Module -> FilePath -> IO Bool
buildExecutable ctx m path =
  withCString path $ \c -> toBool <$> F.c_mtlc_build_executable ctx m c

version :: IO String
version = F.c_mtlc_version >>= peekCString

-- helpers

unwrap :: [Value] -> [F.MtlcValue]
unwrap = map (\(Value v) -> v)

toBool :: CInt -> Bool
toBool = (/= 0)

nonNull :: Ptr a -> Maybe (Ptr a)
nonNull p
  | p == nullPtr = Nothing
  | otherwise = Just p

-- | withCString over a list, keeping every buffer alive for the continuation.
withCStrings :: [String] -> ([CString] -> IO a) -> IO a
withCStrings [] k = k []
withCStrings (s : ss) k =
  withCString s $ \c -> withCStrings ss $ \cs -> k (c : cs)
