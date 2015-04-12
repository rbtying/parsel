module Generators2 where

import {-# SOURCE #-} Generators
import Data.List
import AST

genExpr :: Expr -> [Char]

-- Q: how are these types expressed in C ? (time literal?)
-- guard for each unit type? 
-- is this rhs of a definition?
-- A: we will use floats, and semantic analysis will add functions for "type"
--    conversions
genExpr (Literal val unit) = constToLambda . show $ val

genExpr (Str s) = constToLambda s

genExpr (Attr expr (Symbol sym)) = "(" ++ genExpr expr ++ ")" ++ "." ++ sym 

genExpr (Tuple exprs) = "std::make_tuple(" ++ es ++ ")"
    where es = intercalate ", " $ map genExpr exprs

genExpr (List exprs) = "{" ++ intercalate ", " (map genExpr exprs) ++ "}"

-- waiting on fill functions
genExpr (BinaryOp binOp expr1 expr2) = genExpr $ Func (opToFunc binOp) [expr1, expr2]
    where   opToFunc Plus           = Var $ Symbol "psl::plus"
            opToFunc Minus          = Var $ Symbol "psl::minus"
            opToFunc Divide         = Var $ Symbol "psl::divide"
            opToFunc Multiply       = Var $ Symbol "psl::multiply"
            opToFunc LessThan       = Var $ Symbol "psl::lessThan"
            opToFunc GreaterThan    = Var $ Symbol "psl::greaterThan"
            opToFunc LessThanEq     = Var $ Symbol "psl::lessThanEq"
            opToFunc GreaterThanEq  = Var $ Symbol "psl::greaterThanEq"
            opToFunc Eq             = Var $ Symbol "psl::eq"
            opToFunc And            = Var $ Symbol "psl::and"
            opToFunc Or             = Var $ Symbol "psl::or"

genExpr (UnaryOp unOp expr) = "(unaryop unop expr)"

genExpr (Func expr exprs) = genExpr expr ++ "(" ++ es ++ ")"
    where es = intercalate ", " $ map ((++"()") . genExpr) exprs

genExpr (Var (Symbol sym)) = sym

genExpr (Lambda tsyms t expr) = "lambda tsyms t expr"

genExpr (LetExp ds expr) = "[&]() {" ++ n:decs ++ n:defs ++ n:out ++ n:"}()"
    where   (decs, defs) = genDefs ds
            out = "return " ++ genExpr expr ++ ";"
            n = '\n'

genExpr (Cond expr1 expr2 expr3) = "[&]() {\n" ++ cond ++ "\n}"
    where   cond = "if(" ++ e1 ++ "()) {\n" ++ e2 ++ "}\nelse {\n" ++ e3 ++ "}"
            e1 = genExpr expr1
            e2 = "return " ++ genExpr expr2 ++ ";\n"
            e3 = "return " ++ genExpr expr3 ++ ";\n"


constToLambda :: [Char] -> [Char]
constToLambda e = "[]() { return " ++ e ++ "; }"
