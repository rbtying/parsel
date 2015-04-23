module AST where

type AST        = [TopDef]

data TopDef     = Def Def
                | Struct Symbol Tsyms

data Def        = FuncDef Symbol Tsyms Type IndExpr
                | VarDef Tsym IndExpr
                deriving (Show)

data Symbol     = Symbol String
                deriving (Show, Eq)

instance Ord Symbol where
    (Symbol sym1) `compare` (Symbol sym2) = sym1 `compare` sym2

type IndExpr    = (Expr, Int)

data Expr       = Literal Float Unit
                | Attr IndExpr Symbol
                | Tuple [IndExpr]
                | List [IndExpr]
                | BinaryOp BinOp IndExpr IndExpr
                | UnaryOp UnOp IndExpr
                | Func IndExpr [IndExpr]
                | Var Symbol
                | Lambda Tsyms Type IndExpr
                | LetExp [Def] IndExpr
                | Cond IndExpr IndExpr IndExpr
                | Str [Char]
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
instance Ord Type where
    (Type sym1) `compare` (Type sym2) =
        sym1 `compare` sym2
    (ListType t1) `compare` (ListType t2) =
        t1 `compare` t2
    (TupleType ts1) `compare` (TupleType ts2) =
        ts1 `compare` ts2
    (FuncType ts1 t1) `compare` (FuncType ts2 t2) =
        (t1:ts1) `compare` (t2:ts2)
