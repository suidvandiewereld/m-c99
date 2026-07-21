-- | The C frontend's type system (src/ctype.c). Not libmtlc's types — those
-- are produced at lowering time (see "C99.Lower").
--
-- The C original gives struct/union/enum types identity by Type* pointer:
-- @type_equal@ compares aggregates nominally, and a tag is completed by
-- mutating the Type in place after its members are known. Haskell values have
-- no identity, so aggregates are referenced by a 'TagId' into the
-- 'TypeContext' tag table, and completing a tag rebinds it there. Nominal
-- equality then falls out of comparing TagIds.
--
-- Because layout lives in the context, 'typeSize' and 'typeAlign' are
-- functions of the context rather than fields on the type.
module C99.CType
  ( -- * Types
    Type (..)
  , TagId
  , Member (..)
  , TagInfo (..)
  , TagKind (..)
    -- * Context
  , TypeContext (..)
  , newTypeContext
  , tagInfo
  , tagLookup
  , tagDeclare
  , tagDeclareFresh
  , tagSetMembers
    -- * Layout
  , typeSize
  , typeAlign
  , typeIsComplete
    -- * Predicates
  , typeIsInteger
  , typeIsUnsigned
  , typeIsFloat
  , typeIsArithmetic
  , typeIsScalar
  , typeIsPointerLike
  , typeIsAggregate
    -- * Relations
  , typeEqual
  , typeCompatible
  , typeDecay
  , typePromote
  , typeUsualArith
  , typeIntRank
    -- * Members
  , findMember
  , typeToString
  ) where

import Data.Bits ((.&.), complement)
import qualified Data.Map.Strict as M

-- | A struct, union, or enum tag. Identity, not structure: two TStructs are
-- the same type exactly when their TagIds match, which is what C's nominal
-- aggregate equality means.
type TagId = Int

data Type
  = TVoid
  | TBool
  | TChar -- plain char; signed, as in the C frontend
  | TSChar
  | TUChar
  | TShort
  | TUShort
  | TInt
  | TUInt
  | TLong
  | TULong
  | TLLong
  | TULLong
  | TFloat
  | TDouble
  | TLDouble -- lowered as double
  | TPtr Type
  | -- | Element type, length (0 = incomplete/unsized), and whether the bound
    -- was a VLA expression. A VLA with a constant bound is demoted to a fixed
    -- array by sema; anything else is diagnosed.
    TArray Type !Int !Bool
  | TFunc
      { funcRet :: Type
      , funcParams :: [Type]
      , funcVariadic :: !Bool
      , -- | An empty @()@ parameter list: unspecified, not "no parameters".
        funcOldStyle :: !Bool
      }
  | TStruct !TagId -- struct or union; see TagInfo
  | TEnum !TagId
  | TComplex Type -- _Complex float/double
  deriving (Eq, Show)

data Member = Member
  { memName :: Maybe String -- Nothing for an anonymous struct/union member
  , memType :: Type
  , memOffset :: !Int
  , -- | @Just (bitOffset, bitWidth)@ for a bit-field.
    memBits :: Maybe (Int, Int)
  }
  deriving (Eq, Show)

data TagKind = KStruct | KUnion | KEnum
  deriving (Eq, Ord, Show)

data TagInfo = TagInfo
  { tagKind :: !TagKind
  , tagName :: Maybe String
  , tagMembers :: [Member]
  , tagSize :: !Int
  , tagAlign :: !Int
  , tagIncomplete :: !Bool
  }
  deriving (Eq, Show)

data TypeContext = TypeContext
  { tcTags :: M.Map TagId TagInfo
  , -- | (kind, name) -> id, for looking a tag up by spelling. Unions and
    -- structs share the C namespace but not the entry: @struct S@ and
    -- @union S@ are distinct.
    tcTagNames :: M.Map (TagKind, String) TagId
  , tcNextTag :: !TagId
  }
  deriving (Show)

newTypeContext :: TypeContext
newTypeContext = TypeContext M.empty M.empty 0

tagInfo :: TypeContext -> TagId -> TagInfo
tagInfo tc tid = case M.lookup tid (tcTags tc) of
  Just ti -> ti
  Nothing -> error ("C99.CType: unknown tag id " ++ show tid)

tagLookup :: TypeContext -> TagKind -> String -> Maybe TagId
tagLookup tc k name = M.lookup (k, name) (tcTagNames tc)

-- | Introduce a tag (incomplete). A named tag that already exists keeps its
-- id, so forward references such as @struct node *next;@ resolve to the same
-- type once the tag is completed.
tagDeclare :: TagKind -> Maybe String -> TypeContext -> (TagId, TypeContext)
tagDeclare k mname tc
  | Just name <- mname
  , Just tid <- tagLookup tc k name =
      (tid, tc)
  | otherwise = tagDeclareFresh k mname tc

-- | Introduce a tag under a new id even when the name is already taken.
--
-- The parser uses this for a definition that shadows an outer tag: tag names
-- are scoped, so @struct T { int y; }@ inside a block must not overwrite the
-- @struct T@ the enclosing scope defined. The name now maps to the new id;
-- types already built from the old id keep their members.
tagDeclareFresh :: TagKind -> Maybe String -> TypeContext -> (TagId, TypeContext)
tagDeclareFresh k mname tc =
      let tid = tcNextTag tc
          info =
            TagInfo
              { tagKind = k
              , tagName = mname
              , tagMembers = []
              , tagSize = 0
              , tagAlign = 1
              , tagIncomplete = True
              }
          tc' =
            tc
              { tcTags = M.insert tid info (tcTags tc)
              , tcTagNames = case mname of
                  Just name -> M.insert (k, name) tid (tcTagNames tc)
                  Nothing -> tcTagNames tc
              , tcNextTag = tid + 1
              }
       in (tid, tc')

-- | Complete a struct/union tag: lay its members out and record the result.
-- The layout rules are type_struct_finish's.
tagSetMembers :: TagId -> [Member] -> TypeContext -> TypeContext
tagSetMembers tid members tc =
  let info = tagInfo tc tid
      isUnion = tagKind info == KUnion
      (laid, size, align) = layout tc isUnion members
      info' =
        info
          { tagMembers = laid
          , tagSize = size
          , tagAlign = align
          , tagIncomplete = False
          }
   in tc {tcTags = M.insert tid info' (tcTags tc)}

-- | Struct/union layout, including the bit-field packing the C frontend does:
-- bit-fields pack into 4-byte units, a zero-width field forces a new unit, and
-- a non-bit-field member closes the current unit.
layout :: TypeContext -> Bool -> [Member] -> ([Member], Int, Int)
layout tc isUnion = go 0 1 0 Nothing []
  where
    -- off: next free byte. maxAlign/maxSize: running maxima (maxSize for unions).
    -- unit: Just (unitOff, bitsUsed) while inside a bit-field storage unit.
    go off maxAlign maxSize unit acc [] =
      let off' = case unit of
            Just (uoff, _) -> uoff + 4
            Nothing -> off
          align = max 1 maxAlign
          size =
            if isUnion
              then alignUp maxSize align
              else alignUp off' align
       in (reverse acc, size, align)
    go off maxAlign maxSize unit acc (m : ms) = case memBits m of
      Just (_, 0) ->
        -- zero-width: close the current unit, emit nothing
        case unit of
          Just (uoff, bits) | bits > 0 -> go (uoff + 4) maxAlign maxSize Nothing acc ms
          _ -> go off maxAlign maxSize Nothing acc ms
      Just (_, w)
        | isUnion ->
            let m' = m {memOffset = 0, memBits = Just (0, w)}
             in go off (max 4 maxAlign) (max 4 maxSize) unit (m' : acc) ms
        | otherwise ->
            let (uoff, bits) = case unit of
                  Just (u, b) | b + w <= 32 -> (u, b)
                  Just (u, _) -> (alignUp (u + 4) 4, 0)
                  Nothing -> (alignUp off 4, 0)
                m' = m {memOffset = uoff, memBits = Just (bits, w)}
             in go off (max 4 maxAlign) maxSize (Just (uoff, bits + w)) (m' : acc) ms
      Nothing ->
        let off0 = case unit of
              Just (uoff, _) -> uoff + 4
              Nothing -> off
            mt = memType m
         in if not (typeIsComplete tc mt)
              -- An incomplete member contributes no size, but it stays in the
              -- list: `int mix[KIND_COUNT];` where the bound is a sizeof
              -- expression the parse-time folder cannot evaluate lands here,
              -- and dropping it would make the member invisible to lookup.
              --
              -- It still needs a real offset. Leaving it at 0 put a flexible
              -- array member on top of the first member of the struct, so
              -- writing d[0] destroyed n in `struct { int n; int d[]; }`.
              -- C99 6.7.2.1p16 places it where it would go if the array had
              -- one element, while the struct's own size ignores it.
              then
                let ea = incompleteAlign tc mt
                    o = alignUp off0 ea
                 in go off0 (max ea maxAlign) maxSize Nothing (m {memOffset = o} : acc) ms
              else
                let ma = typeAlign tc mt
                    ms' = typeSize tc mt
                 in if isUnion
                      then
                        let m' = m {memOffset = 0}
                         in go off0 (max ma maxAlign) (max ms' maxSize) Nothing (m' : acc) ms
                      else
                        let o = alignUp off0 ma
                            m' = m {memOffset = o}
                         in go (o + ms') (max ma maxAlign) maxSize Nothing (m' : acc) ms

-- | The alignment an incomplete member wants.
--
-- For a flexible array member that is its element's alignment, which is what
-- decides where the data actually starts. An incomplete type with no element
-- to ask about falls back to 1, so it lands at the current offset rather than
-- pushing anything around.
incompleteAlign :: TypeContext -> Type -> Int
incompleteAlign tc t = case t of
  TArray el _ _ -> typeAlign tc el
  _ -> 1

alignUp :: Int -> Int -> Int
alignUp x a
  | a <= 1 = x
  | otherwise = (x + a - 1) .&. complement (a - 1)

-- ---- layout queries ----

-- | Sizes follow Windows LLP64, which is what the libmtlc x86-64/PE target
-- wants: long is 4 bytes, pointers 8.
typeSize :: TypeContext -> Type -> Int
typeSize tc t = case t of
  TVoid -> 0
  TBool -> 1
  TChar -> 1
  TSChar -> 1
  TUChar -> 1
  TShort -> 2
  TUShort -> 2
  TInt -> 4
  TUInt -> 4
  TLong -> 4 -- LLP64
  TULong -> 4
  TLLong -> 8
  TULLong -> 8
  TFloat -> 4
  TDouble -> 8
  TLDouble -> 8
  TPtr _ -> 8
  TArray e n _
    | n > 0 && typeIsComplete tc e -> typeSize tc e * n
    | otherwise -> 0
  TFunc {} -> 1
  TStruct tid -> tagSize (tagInfo tc tid)
  TEnum _ -> 4
  TComplex b -> 2 * typeSize tc b

typeAlign :: TypeContext -> Type -> Int
typeAlign tc t = case t of
  TVoid -> 1
  TPtr _ -> 8
  TArray e _ _ -> typeAlign tc e
  TFunc {} -> 1
  TStruct tid -> tagAlign (tagInfo tc tid)
  TEnum _ -> 4
  TComplex b -> typeAlign tc b
  _ -> max 1 (typeSize tc t)

typeIsComplete :: TypeContext -> Type -> Bool
typeIsComplete tc t = case t of
  TVoid -> False
  TArray e n _ -> n > 0 && typeIsComplete tc e
  TStruct tid -> not (tagIncomplete (tagInfo tc tid))
  _ -> True

-- ---- predicates ----

typeIsInteger :: Type -> Bool
typeIsInteger t = case t of
  TBool -> True
  TChar -> True
  TSChar -> True
  TUChar -> True
  TShort -> True
  TUShort -> True
  TInt -> True
  TUInt -> True
  TLong -> True
  TULong -> True
  TLLong -> True
  TULLong -> True
  TEnum _ -> True
  _ -> False

typeIsUnsigned :: Type -> Bool
typeIsUnsigned t = case t of
  TBool -> True
  TUChar -> True
  TUShort -> True
  TUInt -> True
  TULong -> True
  TULLong -> True
  _ -> False

typeIsFloat :: Type -> Bool
typeIsFloat t = t == TFloat || t == TDouble || t == TLDouble

typeIsArithmetic :: Type -> Bool
typeIsArithmetic t = typeIsInteger t || typeIsFloat t

typeIsScalar :: Type -> Bool
typeIsScalar t = typeIsArithmetic t || isPtr t
  where
    isPtr (TPtr _) = True
    isPtr _ = False

typeIsPointerLike :: Type -> Bool
typeIsPointerLike t = case t of
  TPtr _ -> True
  TArray {} -> True
  _ -> False

-- | Struct, union, array, or complex: lives in memory, never in a register as
-- far as the frontend is concerned.
typeIsAggregate :: Type -> Bool
typeIsAggregate t = case t of
  TStruct _ -> True
  TArray {} -> True
  TComplex _ -> True
  _ -> False

-- ---- relations ----

typeEqual :: Type -> Type -> Bool
typeEqual a b = case (a, b) of
  (TPtr x, TPtr y) -> typeEqual x y
  (TArray x n _, TArray y m _) -> n == m && typeEqual x y
  (TFunc r1 p1 v1 _, TFunc r2 p2 v2 _) ->
    typeEqual r1 r2
      && v1 == v2
      && length p1 == length p2
      && and (zipWith typeEqual p1 p2)
  (TStruct x, TStruct y) -> x == y -- nominal
  (TEnum x, TEnum y) -> x == y
  (TComplex x, TComplex y) -> typeEqual x y
  _ -> a == b

-- | Assignment compatibility, as loose as the C frontend's: any integer
-- converts to any integer, any float to any float, and void* to anything.
typeCompatible :: Type -> Type -> Bool
typeCompatible a b
  | typeEqual a b = True
  | TPtr x <- a
  , TPtr y <- b =
      x == TVoid || y == TVoid || typeCompatible x y
  | typeIsInteger a && typeIsInteger b = True
  | typeIsFloat a && typeIsFloat b = True
  | otherwise = False

-- | Array-to-pointer and function-to-pointer decay.
typeDecay :: Type -> Type
typeDecay t = case t of
  TArray e _ _ -> TPtr e
  TFunc {} -> TPtr t
  _ -> t

typeIntRank :: Type -> Int
typeIntRank t = case t of
  TBool -> 1
  TChar -> 2
  TSChar -> 2
  TUChar -> 2
  TShort -> 3
  TUShort -> 3
  TInt -> 4
  TUInt -> 4
  TEnum _ -> 4
  TLong -> 5
  TULong -> 5
  TLLong -> 6
  TULLong -> 6
  _ -> 0

-- | The integer promotions: anything of rank below int becomes int.
typePromote :: Type -> Type
typePromote t
  | not (typeIsInteger t) = t
  | typeIntRank t < typeIntRank TInt = TInt
  | otherwise = t

-- | The usual arithmetic conversions (C99 6.3.1.8).
typeUsualArith :: TypeContext -> Type -> Type -> Type
typeUsualArith tc a0 b0
  | typeIsFloat a || typeIsFloat b =
      if a == TLDouble || b == TLDouble
        then TLDouble
        else
          if a == TDouble || b == TDouble
            then TDouble
            else TFloat
  | typeEqual a b = a
  | au == bu = if ar >= br then a else b
  | typeIntRank u >= typeIntRank s = u
  | typeSize tc s > typeSize tc u = s
  | otherwise = case s of
      TInt -> TUInt
      TLong -> TULong
      TLLong -> TULLong
      _ -> u
  where
    a = typePromote a0
    b = typePromote b0
    au = typeIsUnsigned a
    bu = typeIsUnsigned b
    ar = typeIntRank a
    br = typeIntRank b
    u = if au then a else b -- the unsigned one
    s = if au then b else a -- the signed one

-- ---- members ----

-- | Find a member by name, descending into anonymous struct/union members
-- (C11 6.7.2.1p13). The returned Member's offset is relative to the outer
-- aggregate, so callers can address it directly.
findMember :: TypeContext -> Type -> String -> Maybe Member
findMember tc (TStruct tid) name = search (tagMembers (tagInfo tc tid))
  where
    search ms = case filter ((== Just name) . memName) ms of
      (m : _) -> Just m
      [] -> anon ms
    anon [] = Nothing
    anon (m : ms)
      | Nothing <- memName m
      , TStruct _ <- memType m
      , Just inner <- findMember tc (memType m) name =
          Just inner {memOffset = memOffset inner + memOffset m}
      | otherwise = anon ms
findMember _ _ _ = Nothing

typeToString :: TypeContext -> Type -> String
typeToString tc t = case t of
  TVoid -> "void"
  TBool -> "_Bool"
  TChar -> "char"
  TSChar -> "signed char"
  TUChar -> "unsigned char"
  TShort -> "short"
  TUShort -> "unsigned short"
  TInt -> "int"
  TUInt -> "unsigned int"
  TLong -> "long"
  TULong -> "unsigned long"
  TLLong -> "long long"
  TULLong -> "unsigned long long"
  TFloat -> "float"
  TDouble -> "double"
  TLDouble -> "long double"
  TPtr b -> typeToString tc b ++ "*"
  TArray b n _ -> typeToString tc b ++ "[" ++ show n ++ "]"
  TFunc {funcRet = r} -> "fn returning " ++ typeToString tc r
  TStruct tid ->
    let info = tagInfo tc tid
        kw = if tagKind info == KUnion then "union" else "struct"
     in maybe kw ((kw ++ " ") ++) (tagName info)
  TEnum tid -> maybe "enum" ("enum " ++) (tagName (tagInfo tc tid))
  TComplex b -> "_Complex " ++ typeToString tc b
