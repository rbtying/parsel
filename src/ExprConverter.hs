module ExprConverter where

import AST

binOpToFunc :: BinOp -> IndExpr -> IndExpr -> Expr
binOpToFunc binOp ie1 ie2 = Func funcBod [ie1, ie2]
    where   funcBod = ((Var . Symbol . opToString $ binOp), -1)
            opToString Plus           = "psl::plus"
            opToString Minus          = "psl::minus"
            opToString Divide         = "psl::divide"
            opToString Multiply       = "psl::multiply"
            opToString LessThan       = "psl::lessThan"
            opToString GreaterThan    = "psl::greaterThan"
            opToString LessThanEq     = "psl::lessThanEq"
            opToString GreaterThanEq  = "psl::greaterThanEq"
            opToString Eq             = "psl::eq"
            opToString And            = "psl::and"
            opToString Or             = "psl::or"
