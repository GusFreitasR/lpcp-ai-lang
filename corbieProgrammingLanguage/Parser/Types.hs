module Parser.Types where

import Scanner
import Text.Parsec (ParsecT)

type SymbolTable = [(Token, Token)]

data ListShape
  = ShapeKnown [Int]
  | ShapeUnknown
  | ShapeIrregular
  deriving (Eq, Show)

data CorbieType
  = TInt
  | TFloat
  | TBool
  | TString
  | TList ListShape
  | TDict
  | TModel String
  | TEnum String
  | TDynamic
  deriving (Eq, Show)

data ParserState = ParserState
  { symTable     :: SymbolTable
  , scopeStack   :: [String]
  , scopeCounter :: Int
  , varTypes     :: [(String, CorbieType)]
  , constSet     :: [String]
  , constZeros   :: [String]
  , funcSigs     :: [(String, (Int, Maybe Int, CorbieType))]
  , inFunction   :: Bool
  } deriving (Eq, Show)

type Parser a = ParsecT [Token] ParserState IO a

data Program = Program [Stmt] deriving (Eq, Show)

data Stmt
  = FuncDefStmt String [String] [Stmt]
  | ModelDefStmt String [Stmt]
  | EnumDefStmt String [String]
  | ConstDeclStmt String Expr
  | AssignStmt Assign
  | FuncCallStmt FuncCall
  | IfStmt Expr [Stmt] (Maybe [Stmt])
  | WhileStmt Expr [Stmt]
  | ForStmt Assign Expr Assign [Stmt]
  | ForInStmt String Expr [Stmt]
  | ReturnStmt (Maybe Expr)
  | PrintStmt Expr
  deriving (Eq, Show)

data Assign = AssignNode Var AssignOp Expr deriving (Eq, Show)

data AssignOp = OpAssign | OpPlusAssign | OpMinusAssign | OpMultAssign | OpDivAssign
  deriving (Eq, Show)

data Var = SimpleVar String | IndexVar Var [Slice] deriving (Eq, Show)

data Slice = SliceSingle Expr | SliceFromTo (Maybe Expr) (Maybe Expr)
  deriving (Eq, Show)

data FuncCall = CallFunc String [Expr] deriving (Eq, Show)

data Expr
  = LitExpr Literal
  | VarExpr Var
  | FuncCallExpr FuncCall
  | ListExpr [Expr]
  | ListCompExpr Expr String Expr (Maybe Expr)
  | DictExpr [(String, Expr)]
  | BinaryExpr BinOp Expr Expr
  | UnaryExpr UnOp Expr
  deriving (Eq, Show)

data BinOp
  = OpOr | OpAnd
  | OpEqual | OpNotEqual | OpLess | OpLessEqual | OpGreater | OpGreaterEqual | OpIn
  | OpAdd | OpSub | OpCat
  | OpMul | OpDiv | OpMod | OpMatMul
  | OpPow
  deriving (Eq, Show)

data UnOp = OpNot | OpNeg | OpPos deriving (Eq, Show)

data Literal = IntLit Int | FloatLit Double | StringLit String | BoolLit Bool
  deriving (Eq, Show)
