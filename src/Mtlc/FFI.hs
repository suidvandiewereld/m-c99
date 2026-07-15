-- | Raw foreign imports for libmtlc (libmtlc/include/mtlc/*.h).
--
-- One import per C entry point, nothing more: marshalling and the pleasant
-- interface live in "Mtlc". The C frontend in src/ uses exactly the surface
-- bound here, so anything it can build, this can build.
{-# LANGUAGE CApiFFI #-}
module Mtlc.FFI where

import Foreign.C.String (CString)
import Foreign.C.Types (CDouble (..), CInt (..), CLLong (..), CSize (..))
import Foreign.Ptr (Ptr)

-- Opaque handles. Phantom-tagged pointers so the type checker keeps them apart.
data MtlcBuilder_
data MtlcFn_
data MtlcModule_
data MtlcContext_
data MtlcType_

type BuilderPtr = Ptr MtlcBuilder_
type FnPtr' = Ptr MtlcFn_
type ModulePtr = Ptr MtlcModule_
type ContextPtr = Ptr MtlcContext_
type TypePtr = Ptr MtlcType_

-- | An SSA temporary / storage handle within one function builder.
-- @MTLC_NO_VALUE@ is (-1).
type MtlcValue = CInt

noValue :: MtlcValue
noValue = -1

-- MtlcTypeKind (mtlc/type.h), in declaration order.
mtlcInt8, mtlcInt16, mtlcInt32, mtlcInt64 :: CInt
mtlcInt8 = 0
mtlcInt16 = 1
mtlcInt32 = 2
mtlcInt64 = 3

mtlcUint8, mtlcUint16, mtlcUint32, mtlcUint64 :: CInt
mtlcUint8 = 4
mtlcUint16 = 5
mtlcUint32 = 6
mtlcUint64 = 7

mtlcBool, mtlcFloat32, mtlcFloat64 :: CInt
mtlcBool = 8
mtlcFloat32 = 9
mtlcFloat64 = 10

mtlcArray, mtlcVoid :: CInt
mtlcArray = 14
mtlcVoid = 18

-- ---- types ----

foreign import ccall unsafe "mtlc_type_scalar"
  c_mtlc_type_scalar :: CInt -> IO TypePtr

foreign import ccall unsafe "mtlc_type_pointer"
  c_mtlc_type_pointer :: TypePtr -> IO TypePtr

-- | From cbits/blob.c, not libmtlc: an interned array-of-uint8 descriptor,
-- which build.h offers no constructor for.
foreign import ccall unsafe "c99m_blob_type"
  c_c99m_blob_type :: CSize -> IO TypePtr

-- ---- builder / module ----

foreign import ccall unsafe "mtlc_builder_create"
  c_mtlc_builder_create :: IO BuilderPtr

foreign import ccall unsafe "mtlc_builder_destroy"
  c_mtlc_builder_destroy :: BuilderPtr -> IO ()

foreign import ccall unsafe "mtlc_builder_function"
  c_mtlc_builder_function
    :: BuilderPtr
    -> CString          -- name
    -> TypePtr          -- return type
    -> Ptr CString      -- param names
    -> Ptr TypePtr      -- param types
    -> CSize            -- param count
    -> CInt             -- is_extern
    -> IO FnPtr'

foreign import ccall unsafe "mtlc_builder_global"
  c_mtlc_builder_global
    :: BuilderPtr -> CString -> TypePtr -> CLLong -> CInt -> IO ()

foreign import ccall unsafe "mtlc_builder_finish"
  c_mtlc_builder_finish :: BuilderPtr -> IO ModulePtr

foreign import ccall unsafe "mtlc_module_function_count"
  c_mtlc_module_function_count :: ModulePtr -> IO CSize

foreign import ccall unsafe "mtlc_module_destroy"
  c_mtlc_module_destroy :: ModulePtr -> IO ()

-- ---- values ----

foreign import ccall unsafe "mtlc_fn_param"
  c_mtlc_fn_param :: FnPtr' -> CSize -> IO MtlcValue

foreign import ccall unsafe "mtlc_const_int"
  c_mtlc_const_int :: FnPtr' -> TypePtr -> CLLong -> IO MtlcValue

foreign import ccall unsafe "mtlc_const_float"
  c_mtlc_const_float :: FnPtr' -> TypePtr -> CDouble -> IO MtlcValue

foreign import ccall unsafe "mtlc_local"
  c_mtlc_local :: FnPtr' -> CString -> TypePtr -> IO MtlcValue

foreign import ccall unsafe "mtlc_global_ref"
  c_mtlc_global_ref :: FnPtr' -> CString -> IO MtlcValue

-- ---- instructions ----

foreign import ccall unsafe "mtlc_assign"
  c_mtlc_assign :: FnPtr' -> MtlcValue -> MtlcValue -> IO ()

foreign import ccall unsafe "mtlc_binary"
  c_mtlc_binary
    :: FnPtr' -> CString -> MtlcValue -> MtlcValue -> TypePtr -> IO MtlcValue

foreign import ccall unsafe "mtlc_unary"
  c_mtlc_unary :: FnPtr' -> CString -> MtlcValue -> TypePtr -> IO MtlcValue

foreign import ccall unsafe "mtlc_call"
  c_mtlc_call
    :: FnPtr' -> CString -> Ptr MtlcValue -> CSize -> TypePtr -> IO MtlcValue

foreign import ccall unsafe "mtlc_function_address"
  c_mtlc_function_address :: FnPtr' -> CString -> IO MtlcValue

foreign import ccall unsafe "mtlc_call_indirect"
  c_mtlc_call_indirect
    :: FnPtr' -> MtlcValue -> Ptr MtlcValue -> CSize -> TypePtr -> IO MtlcValue

foreign import ccall unsafe "mtlc_cast"
  c_mtlc_cast :: FnPtr' -> MtlcValue -> TypePtr -> IO MtlcValue

foreign import ccall unsafe "mtlc_address_of"
  c_mtlc_address_of :: FnPtr' -> MtlcValue -> TypePtr -> IO MtlcValue

foreign import ccall unsafe "mtlc_load"
  c_mtlc_load :: FnPtr' -> MtlcValue -> TypePtr -> IO MtlcValue

foreign import ccall unsafe "mtlc_store"
  c_mtlc_store :: FnPtr' -> MtlcValue -> MtlcValue -> TypePtr -> IO ()

-- ---- control flow ----

foreign import ccall unsafe "mtlc_label"
  c_mtlc_label :: FnPtr' -> CString -> IO ()

foreign import ccall unsafe "mtlc_jump"
  c_mtlc_jump :: FnPtr' -> CString -> IO ()

foreign import ccall unsafe "mtlc_branch_if_zero"
  c_mtlc_branch_if_zero :: FnPtr' -> MtlcValue -> CString -> IO ()

foreign import ccall unsafe "mtlc_return"
  c_mtlc_return :: FnPtr' -> MtlcValue -> IO ()

-- ---- context / pipeline ----

foreign import ccall unsafe "mtlc_context_create"
  c_mtlc_context_create :: IO ContextPtr

foreign import ccall unsafe "mtlc_context_destroy"
  c_mtlc_context_destroy :: ContextPtr -> IO ()

foreign import ccall unsafe "mtlc_context_set_opt_level"
  c_mtlc_context_set_opt_level :: ContextPtr -> CInt -> IO ()

foreign import ccall unsafe "mtlc_context_set_whole_program"
  c_mtlc_context_set_whole_program :: ContextPtr -> CInt -> IO ()

-- These run the optimizer / codegen / PE linker: seconds of work, and they can
-- allocate heavily. 'safe' so the RTS is not blocked for the duration.
foreign import ccall safe "mtlc_optimize"
  c_mtlc_optimize :: ContextPtr -> ModulePtr -> IO CInt

foreign import ccall safe "mtlc_emit_object"
  c_mtlc_emit_object :: ContextPtr -> ModulePtr -> CString -> IO CInt

foreign import ccall safe "mtlc_build_executable"
  c_mtlc_build_executable :: ContextPtr -> ModulePtr -> CString -> IO CInt

foreign import ccall unsafe "mtlc_version"
  c_mtlc_version :: IO CString
