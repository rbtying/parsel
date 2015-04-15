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

genExpr (BinaryOp binOp expr1 expr2) =
    genExpr $ Func (Var . Symbol . opToFunc $ binOp) [expr1, expr2]
    where   opToFunc Plus           = "psl::plus"
            opToFunc Minus          = "psl::minus"
            opToFunc Divide         = "psl::divide"
            opToFunc Multiply       = "psl::multiply"
            opToFunc LessThan       = "psl::lessThan"
            opToFunc GreaterThan    = "psl::greaterThan"
            opToFunc LessThanEq     = "psl::lessThanEq"
            opToFunc GreaterThanEq  = "psl::greaterThanEq"
            opToFunc Eq             = "psl::eq"
            opToFunc And            = "psl::and"
            opToFunc Or             = "psl::or"
            

genExpr (UnaryOp unOp expr) =
    genExpr $ Func (Var . Symbol . opToFunc $ unOp) [expr]
    where opToFunc Negate = "psl::negate"

genExpr (Func expr exprs) = genExpr expr ++ "(" ++ es ++ ")"
    where es = intercalate ", " $ map genRawExpr exprs

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


genRawExpr :: Expr -> [Char]
genRawExpr (Literal val unit) = show val
genRawExpr (Func expr exprs) = genExpr (Func expr exprs)
genRawExpr (BinaryOp op expr1 expr2) = genExpr (BinaryOp op expr1 expr2)
genRawExpr (UnaryOp op expr) = genExpr (UnaryOp op expr)
genRawExpr (Str s) = s
genRawExpr e = genExpr e ++ "()"


constToLambda :: [Char] -> [Char]
constToLambda e = "[]() { return " ++ e ++ "; }"
