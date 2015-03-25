module AST where

type Defs       = [Def]


data Def        = FuncDef Symbol Tsyms Type Expr
                | VarDef Symbol Expr
                deriving (Show)

data Symbol     = Symbol String
                deriving (Show, Eq)

data Expr       = Literal Float Unit
                | Attr Expr Symbol
                | Tuple Args
                | List Args
                | BinaryOp BinOp Expr Expr
                | UnaryOp UnOp Expr
                | Func Symbol Args
                | Var Symbol
                | Lambda Tsyms Type Expr
                | LetExp Defs Expr
                | Cond Expr Expr Expr
                deriving (Show)

type Unit       = String
type Args       = [Expr]

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
                | ListType Symbol
                | FuncType [Type] Type
                deriving (Show, Eq)
