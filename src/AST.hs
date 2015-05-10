module AST where

type AST        = [TopDef]

data TopDef     = Def Def
                | Struct Symbol Tsyms

data Def        = FuncDef Symbol Tsyms Type Expr
                | VarDef Tsym Expr
                deriving (Show)

data Symbol     = Symbol String
                deriving (Show, Eq)

instance Ord Symbol where
    (Symbol sym1) `compare` (Symbol sym2) = sym1 `compare` sym2

data Expr       = Literal Float Unit
                | Attr Expr Symbol
                | Tuple [Expr]
                | List [Expr]
                | BinaryOp BinOp Expr Expr
                | UnaryOp UnOp Expr
                | Func Expr [Expr]
                | Var Symbol Int
                | Lambda Tsyms Type Expr
                | LetExp [Def] Expr
                | Cond Expr Expr Expr
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
    (Type sym1) <= (Type sym2) =
        sym1 <= sym2
    (Type _) <= _ = True

    (ListType t1) <= (ListType t2) =
        t1 <= t2
    (ListType _) <= _ = True

    (TupleType ts1) <= (TupleType ts2) =
        ts1 <= ts2
    (TupleType _) <= _ = True

    (FuncType ts1 t1) <= (FuncType ts2 t2) =
        (t1:ts1) <= (t2:ts2)
    (FuncType _ _) <= _ = True
