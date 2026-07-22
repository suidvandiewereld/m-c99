-- | The AST (src/ast.h).
--
-- The C frontend uses one god-struct Node for every expression, statement and
-- declaration, with ~25 fields of which each kind uses a handful. Here they
-- are three separate ADTs, so a malformed node is unrepresentable rather than
-- merely unusual.
--
-- Two fields are filled in by a later pass and so are wrapped in Maybe:
-- 'exTy' (sema assigns every expression a type) and the 'SymId' on identifiers
-- and declarations (sema resolves every name to a symbol). Symbols are
-- referenced by id rather than by value because sema keeps mutating them after
-- the reference is created — @&x@ sets address-taken on x long after x's
-- declaration was walked.
module C99.Ast
  ( -- * Operators
    BinOp (..)
  , UnOp (..)
  , PostOp (..)
  , AssignOp (..)
  , binOpText
    -- * Expressions
  , Expr (..)
  , ExprNode (..)
  , IntSuffix (..)
  , noSuffix
  , Init (..)
  , Designator (..)
  , mkExpr
    -- * Statements
  , Stmt (..)
  , StmtNode (..)
  , BlockItem (..)
    -- * Declarations
  , StorageClass (..)
  , Decl (..)
  , FuncDef (..)
  , Param (..)
  , TopDecl (..)
  , Program
    -- * Symbols
  , SymId
  , SymKind (..)
  , Symbol (..)
  ) where

import C99.Common (SrcLoc)
import C99.CType (TagId, Type)

data BinOp
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | Eq
  | Ne
  | Lt
  | Le
  | Gt
  | Ge
  | LAnd -- &&
  | LOr -- ||
  | BAnd
  | BOr
  | BXor
  | Shl
  | Shr
  deriving (Eq, Show)

data UnOp
  = Not -- !
  | Neg -- unary -
  | UPlus -- unary +
  | BNot -- ~
  | AddrOf -- &
  | Deref -- *
  | PreInc
  | PreDec
  deriving (Eq, Show)

data PostOp = PostInc | PostDec
  deriving (Eq, Show)

-- | @=@, or a compound assignment such as @+=@.
data AssignOp = Assign | AssignOp BinOp
  deriving (Eq, Show)

-- | The libmtlc spelling of a binary operator (mtlc_binary's @op@ string).
binOpText :: BinOp -> String
binOpText op = case op of
  Add -> "+"
  Sub -> "-"
  Mul -> "*"
  Div -> "/"
  Mod -> "%"
  Eq -> "=="
  Ne -> "!="
  Lt -> "<"
  Le -> "<="
  Gt -> ">"
  Ge -> ">="
  LAnd -> "&&"
  LOr -> "||"
  BAnd -> "&"
  BOr -> "|"
  BXor -> "^"
  Shl -> "<<"
  Shr -> ">>"

-- | Which integer type an unsuffixed-or-suffixed literal wants.
data IntSuffix = IntSuffix
  { sufUnsigned :: !Bool
  , -- | 0 = none, 1 = @l@, 2 = @ll@
    sufLong :: !Int
  }
  deriving (Eq, Show)

noSuffix :: IntSuffix
noSuffix = IntSuffix False 0

data Expr = Expr
  { exLoc :: SrcLoc
  , -- | Filled by sema. Nothing before it runs.
    exTy :: Maybe Type
  , exNode :: ExprNode
  }
  deriving (Eq, Show)

mkExpr :: SrcLoc -> ExprNode -> Expr
mkExpr loc node = Expr {exLoc = loc, exTy = Nothing, exNode = node}

data ExprNode
  = EInt !Integer IntSuffix
  | -- | Value, and whether an @f@ suffix made it a float rather than a double.
    EFloat !Double !Bool
  | EChar !Integer
  | EString String
  | -- | The SymId is filled by sema.
    EIdent String (Maybe SymId)
  | EBinary BinOp Expr Expr
  | EUnary UnOp Expr
  | EPostfix PostOp Expr
  | EAssign AssignOp Expr Expr
  | -- | Callee, arguments, and — for @__builtin_va_arg(ap, T)@ only — the
    -- type operand, which is not an expression and cannot live in the list.
    ECall Expr [Expr] (Maybe Type)
  | EIndex Expr Expr
  | -- | Object, member name, and whether the access was written with @->@.
    EMember Expr String !Bool
  | ECast Type Expr
  | ESizeofExpr Expr
  | ESizeofType Type
  | ECond Expr Expr Expr
  | EComma Expr Expr
  | ECompoundLit Type Init
  | -- | Produced by sema, never by the parser: @__builtin_va_start@,
    -- @__builtin_va_end@, @__builtin_va_arg@, @__real__@, @__imag__@, and the
    -- imaginary unit @__c99m_I@. Name, arguments, and the type operand that
    -- only @__builtin_va_arg@ carries.
    EBuiltin String [Expr] (Maybe Type)
  deriving (Eq, Show)

-- | An initializer: either a single expression or a brace-enclosed list whose
-- elements may be designated.
data Init
  = IExpr Expr
  | IList [(Maybe Designator, Init)]
  deriving (Eq, Show)

data Designator
  = DField String -- .name =
  | DIndex Expr -- [const] =
  deriving (Eq, Show)

data Stmt = Stmt
  { stLoc :: SrcLoc
  , stNode :: StmtNode
  }
  deriving (Eq, Show)

data StmtNode
  = SExpr Expr
  | SCompound [BlockItem]
  | SIf Expr Stmt (Maybe Stmt)
  | SWhile Expr Stmt
  | SDo Stmt Expr
  | -- | init, condition, increment, body. The init clause may be a
    -- declaration (C99), hence BlockItem rather than Expr.
    SFor (Maybe BlockItem) (Maybe Expr) (Maybe Expr) Stmt
  | SBreak
  | SContinue
  | SReturn (Maybe Expr)
  | SSwitch Expr Stmt
  | SCase Expr Stmt
  | SDefault Stmt
  | SGoto String
  | SLabel String Stmt
  | SNull
  deriving (Eq, Show)

data BlockItem
  = BIStmt Stmt
  | -- | One declaration statement may declare several names: @int a, *b;@
    BIDecl [Decl]
  deriving (Eq, Show)

data StorageClass
  = ScNone
  | ScTypedef
  | ScExtern
  | ScStatic
  | ScAuto
  | ScRegister
  deriving (Eq, Show)

data Decl = Decl
  { dLoc :: SrcLoc
  , dName :: String
  , dType :: Type
  , dStorage :: !StorageClass
  , dInit :: Maybe Init
  , -- | GCC asm label: @int f() __asm__("real_name")@ overrides the link name.
    dAsmLabel :: Maybe String
  , -- | Every array bound in this declarator that did not fold to a constant,
    -- outermost first, keyed by the id the type carries. Lowering evaluates
    -- each once when the declaration is reached.
    dVlaBounds :: [(Int, Expr)]
  , dSym :: Maybe SymId
  }
  deriving (Eq, Show)

data Param = Param
  { pName :: String
  , pType :: Type
  , -- | As 'dVlaBounds'. A parameter's bounds name earlier parameters, so
    -- lowering evaluates them on entry, in order.
    pVlaBounds :: [(Int, Expr)]
  , pSym :: Maybe SymId
  }
  deriving (Eq, Show)

data FuncDef = FuncDef
  { fdLoc :: SrcLoc
  , fdName :: String
  , -- | The function type; its return type is @funcRet@.
    fdType :: Type
  , fdParams :: [Param]
  , fdStorage :: !StorageClass
  , fdVariadic :: !Bool
  , fdBody :: Stmt
  , fdSym :: Maybe SymId
  }
  deriving (Eq, Show)

data TopDecl
  = TDFunc FuncDef
  | -- | A variable, a function prototype, or a typedef, per its storage class
    -- and type.
    TDDecl Decl
  | -- | @struct S { ... };@ with no declarator: introduces the tag only.
    TDTag TagId
  | -- | An enumerator, hoisted out of its enum so lowering sees it directly.
    TDEnumConst String !Integer Type
  deriving (Eq, Show)

type Program = [TopDecl]

-- ---- symbols (src/sema.h) ----

type SymId = Int

data SymKind
  = SymVar
  | SymFunc
  | SymTypedef
  | SymEnumConst
  deriving (Eq, Show)

data Symbol = Symbol
  { symId :: !SymId
  , symKind :: !SymKind
  , -- | The name as written, which is what lookups use.
    symName :: String
  , -- | The name emitted into the IR. Differs from symName for a file-scope
    -- static (mangled per translation unit) or an @__asm__@ label.
    symLinkName :: String
  , symType :: Type
  , symEnumVal :: !Integer
  , symIsExtern :: !Bool
  , symIsGlobal :: !Bool
  , -- | Internal linkage.
    symIsStatic :: !Bool
  , symIsDefined :: !Bool
  , -- | @&x@ appears somewhere, so x needs addressable storage rather than
    -- living in a register.
    symAddrTaken :: !Bool
  }
  deriving (Eq, Show)
