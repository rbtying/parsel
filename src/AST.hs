module AST where

data AST              = Tuple [AST]
                      | Attr AST Symbol
                      | List [AST]
                      | Let Symbol AST AST
                      | Apply Symbol [AST]
                      | Func Symbol [TypedSymbol] Type AST
                      | Lambda [TypedSymbol] Type AST
                      | Literal String Value
                      | Var Symbol
                      | BinaryOp BinOp AST AST
                      | UnaryOp UnOp AST
                      | Cond AST AST AST
                      | Nop
                      deriving (Show, Eq)

data BinOp            = AddOp
                      | SubOp
                      | MulOp
                      | DivOp
                      | LessThanOp
                      | LessThanEqOp
                      | GreaterThanOp
                      | GreaterThanEqOp
                      | EqOp
                      deriving (Show, Eq)

data UnOp             = NegateOp
                      | NullOp
                      deriving (Show, Eq)

data TypedSymbol      = TypedSymbol Type Symbol
                      deriving (Show, Eq)

data Type             = Type Symbol
                      | ListType Symbol
                      | FuncType [Type] Type
                      deriving (Show, Eq)

data Symbol           = Symbol String
                      deriving (Show, Eq)
data Value            = Value Float
                      deriving (Show, Eq)
