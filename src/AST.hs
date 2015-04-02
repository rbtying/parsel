module AST where

type AST        = [Def]


data Def        = FuncDef Symbol Tsyms Type Expr
                | VarDef Tsym Expr
                | Struct Symbol Tsyms
                deriving (Show)

data Symbol     = Symbol String
                deriving (Show, Eq)

data Expr       = Literal Float Unit
                | Attr Expr Symbol
                | Tuple [Expr]
                | List [Expr]
                | Index Expr Expr
                | BinaryOp BinOp Expr Expr
                | UnaryOp UnOp Expr
                | Func Expr [Expr]
                | Var Symbol
                | Lambda Tsyms Type Expr
                | LetExp [Def] Expr
                | Cond Expr Expr Expr
                | TypeExpr Type
                deriving (Show)

type Unit       = String

data BinOp      = Plus
                | Minus
                | Divide
                | Multiply
                | LessThan
                | GreaterThan
                | LessThanEq
                | GreaterThanEq
                | Eq
                | And
                | Or
                deriving (Show, Eq)

data UnOp       = Negate
                deriving (Show, Eq)

type Tsyms      = [Tsym]

data Tsym       = Tsym Type Symbol
                deriving (Show, Eq)

data Type       = Type Symbol
                | ListType Type
                | TupleType [Type]
                | FuncType [Type] Type
                deriving (Show, Eq)
