module ExprConverter where

import AST

binOpToFunc :: BinOp -> Expr -> Expr -> Expr
binOpToFunc binOp e1 e2 = Func funcBod [e1, e2]
    where   funcBod = Var (Symbol $ opToString binOp) 0
            opToString Plus             = "psl::plus"
            opToString Minus            = "psl::minus"
            opToString Divide           = "psl::divide"
            opToString Multiply         = "psl::multiply"
            opToString LessThan         = "psl::lessThan"
            opToString GreaterThan      = "psl::greaterThan"
            opToString LessThanEq       = "psl::lessThanEq"
            opToString GreaterThanEq    = "psl::greaterThanEq"
            opToString Eq               = "psl::eq"
            opToString And              = "psl::and_"
            opToString Or               = "psl::or_"

unOpToFunc :: UnOp -> Expr -> Expr
unOpToFunc unOp e1 = Func funcBod [e1]
    where   funcBod = Var (Symbol $ opToString unOp) 0
            opToString Negate           = "psl::negate"
