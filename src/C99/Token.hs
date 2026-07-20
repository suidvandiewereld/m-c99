-- | Tokens (src/token.h).
module C99.Token
  ( TokenKind (..)
  , Token (..)
  , emptyToken
  , keywordKind
  , tokenKindName
  ) where

import qualified Data.Map.Strict as M

import C99.Common (SrcLoc, noLoc)

data TokenKind
  = TkEof
  | -- punctuation / operators
    TkLParen
  | TkRParen
  | TkLBrace
  | TkRBrace
  | TkLBracket
  | TkRBracket
  | TkSemi
  | TkComma
  | TkColon
  | TkQuestion
  | TkDot
  | TkArrow
  | TkEllipsis
  | TkPlus
  | TkMinus
  | TkStar
  | TkSlash
  | TkPercent
  | TkAmp
  | TkPipe
  | TkCaret
  | TkTilde
  | TkBang
  | TkLt
  | TkGt
  | TkLe
  | TkGe
  | TkEq
  | TkNe
  | TkLShift
  | TkRShift
  | TkAndAnd
  | TkOrOr
  | TkInc
  | TkDec
  | TkAssign
  | TkAddAssign
  | TkSubAssign
  | TkMulAssign
  | TkDivAssign
  | TkModAssign
  | TkAndAssign
  | TkOrAssign
  | TkXorAssign
  | TkLShiftAssign
  | TkRShiftAssign
  | -- literals & identifiers
    TkIdent
  | TkInt
  | TkFloat
  | TkChar
  | TkString
  | -- keywords
    TkAuto
  | TkBreak
  | TkCase
  | TkCharKw
  | TkConst
  | TkContinue
  | TkDefault
  | TkDo
  | TkDouble
  | TkElse
  | TkEnum
  | TkExtern
  | TkFloatKw
  | TkFor
  | TkGoto
  | TkIf
  | TkInline
  | TkIntKw
  | TkLong
  | TkRegister
  | TkRestrict
  | TkReturn
  | TkShort
  | TkSigned
  | TkSizeof
  | TkStatic
  | TkStruct
  | TkSwitch
  | TkTypedef
  | TkUnion
  | TkUnsigned
  | TkVoid
  | TkVolatile
  | TkWhile
  | TkBool -- _Bool
  | TkComplex -- _Complex
  | TkInt128 -- __int128, lowered to a two-u64 struct
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | A token and its payload. Which payload fields are meaningful depends on
-- the kind: @tokIVal@ for TkInt/TkChar (and the byte length of a TkString),
-- @tokFVal@ for TkFloat, @tokText@ for TkIdent/TkString.
data Token = Token
  { tokKind :: !TokenKind
  , tokLoc :: SrcLoc
  , tokText :: String
  , tokIVal :: !Integer
  , tokFVal :: !Double
  , tokUnsigned :: !Bool -- u suffix
  , tokLong :: !Bool -- l suffix
  , tokLongLong :: !Bool -- ll suffix
  , tokFloatSuf :: !Bool -- f suffix: float rather than double
  }
  deriving (Eq, Show)

emptyToken :: Token
emptyToken =
  Token
    { tokKind = TkEof
    , tokLoc = noLoc
    , tokText = ""
    , tokIVal = 0
    , tokFVal = 0
    , tokUnsigned = False
    , tokLong = False
    , tokLongLong = False
    , tokFloatSuf = False
    }

-- | Keyword lookup; TkIdent when the spelling is not a keyword. The table is a
-- shared Map: this runs for every identifier the lexer produces, so a chain of
-- string comparisons is measurable.
keywordKind :: String -> TokenKind
keywordKind s = M.findWithDefault TkIdent s keywordTable

keywordTable :: M.Map String TokenKind
keywordTable =
  M.fromList
    [ ("auto", TkAuto)
    , ("break", TkBreak)
    , ("case", TkCase)
    , ("char", TkCharKw)
    , ("const", TkConst)
    , ("continue", TkContinue)
    , ("default", TkDefault)
    , ("do", TkDo)
    , ("double", TkDouble)
    , ("else", TkElse)
    , ("enum", TkEnum)
    , ("extern", TkExtern)
    , ("float", TkFloatKw)
    , ("for", TkFor)
    , ("goto", TkGoto)
    , ("if", TkIf)
    , ("inline", TkInline)
    , ("int", TkIntKw)
    , ("long", TkLong)
    , ("register", TkRegister)
    , ("restrict", TkRestrict)
    , ("return", TkReturn)
    , ("short", TkShort)
    , ("signed", TkSigned)
    , ("sizeof", TkSizeof)
    , ("static", TkStatic)
    , ("struct", TkStruct)
    , ("switch", TkSwitch)
    , ("typedef", TkTypedef)
    , ("union", TkUnion)
    , ("unsigned", TkUnsigned)
    , ("void", TkVoid)
    , ("volatile", TkVolatile)
    , ("while", TkWhile)
    , ("_Bool", TkBool)
    , ("_Complex", TkComplex)
    , ("__int128", TkInt128)
    ]

-- | Human-readable spelling, for diagnostics.
tokenKindName :: TokenKind -> String
tokenKindName k = case k of
  TkEof -> "end of file"
  TkLParen -> "("
  TkRParen -> ")"
  TkLBrace -> "{"
  TkRBrace -> "}"
  TkLBracket -> "["
  TkRBracket -> "]"
  TkSemi -> ";"
  TkComma -> ","
  TkColon -> ":"
  TkQuestion -> "?"
  TkDot -> "."
  TkArrow -> "->"
  TkEllipsis -> "..."
  TkPlus -> "+"
  TkMinus -> "-"
  TkStar -> "*"
  TkSlash -> "/"
  TkPercent -> "%"
  TkAmp -> "&"
  TkPipe -> "|"
  TkCaret -> "^"
  TkTilde -> "~"
  TkBang -> "!"
  TkLt -> "<"
  TkGt -> ">"
  TkLe -> "<="
  TkGe -> ">="
  TkEq -> "=="
  TkNe -> "!="
  TkLShift -> "<<"
  TkRShift -> ">>"
  TkAndAnd -> "&&"
  TkOrOr -> "||"
  TkInc -> "++"
  TkDec -> "--"
  TkAssign -> "="
  TkAddAssign -> "+="
  TkSubAssign -> "-="
  TkMulAssign -> "*="
  TkDivAssign -> "/="
  TkModAssign -> "%="
  TkAndAssign -> "&="
  TkOrAssign -> "|="
  TkXorAssign -> "^="
  TkLShiftAssign -> "<<="
  TkRShiftAssign -> ">>="
  TkIdent -> "identifier"
  TkInt -> "integer constant"
  TkFloat -> "floating constant"
  TkChar -> "character constant"
  TkString -> "string literal"
  TkAuto -> "auto"
  TkBreak -> "break"
  TkCase -> "case"
  TkCharKw -> "char"
  TkConst -> "const"
  TkContinue -> "continue"
  TkDefault -> "default"
  TkDo -> "do"
  TkDouble -> "double"
  TkElse -> "else"
  TkEnum -> "enum"
  TkExtern -> "extern"
  TkFloatKw -> "float"
  TkFor -> "for"
  TkGoto -> "goto"
  TkIf -> "if"
  TkInline -> "inline"
  TkIntKw -> "int"
  TkLong -> "long"
  TkRegister -> "register"
  TkRestrict -> "restrict"
  TkReturn -> "return"
  TkShort -> "short"
  TkSigned -> "signed"
  TkSizeof -> "sizeof"
  TkStatic -> "static"
  TkStruct -> "struct"
  TkSwitch -> "switch"
  TkTypedef -> "typedef"
  TkUnion -> "union"
  TkUnsigned -> "unsigned"
  TkVoid -> "void"
  TkVolatile -> "volatile"
  TkWhile -> "while"
  TkBool -> "_Bool"
  TkComplex -> "_Complex"
  TkInt128 -> "__int128"
