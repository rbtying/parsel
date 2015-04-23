module Generators2 where

import {-# SOURCE #-} Generators
import AST

import Data.List


genExpr :: Expr -> [Char]

genExpr (Literal val ('K':_)) = toLambda $ show (val * 1.0e3)
genExpr (Literal val ('M':_)) = toLambda $ show (val * 1.0e6)
genExpr (Literal val ('m':_)) = toLambda $ show (val * 1.0e-3)
genExpr (Literal val ('u':_)) = toLambda $ show (val * 1.0e-6)
genExpr (Literal val _) = toLambda $ show val

genExpr (Str s) = toLambda s

-- this is WRONG! not lazy lewl
genExpr (Attr expr (Symbol sym)) = "(" ++ genRawExpr expr ++ ")" ++ "." ++ sym 

genExpr (Tuple exprs) = toLambda $ "std::make_tuple(" ++ es ++ ")"
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
    where es = intercalate ", " $ map genExpr exprs

genExpr (Var (Symbol sym))
    | sym == "sin"          = "psl::sin"
    | sym == "cos"          = "psl::cos"
    | sym == "ft"           = "psl::ft"
    | sym == "intervalMap"  = "psl::intervalMap"
    | otherwise     = sym

genExpr (Lambda tsyms _ expr) = genLambda tsyms expr

genExpr (LetExp ds expr) = "[&]() {" ++ n:decs ++ n:defs ++ n:out ++ n:"}"
    where   (decs, defs) = genDefs ds
            out = genReturn expr
            n = '\n'

genExpr (Cond expr1 expr2 expr3) = "[&]() {\n" ++ cond ++ "\n}"
    where   cond = "if(" ++ e1 ++ ") {\n" ++ e2 ++ "\n}\nelse {\n" ++ e3 ++ "\n};"
            e1 = genRawExpr expr1
            e2 = genReturn expr2
            e3 = genReturn expr3


genLambda :: Tsyms -> Expr -> [Char]
genLambda tsyms expr = "[&](" ++ args ++ ") {\n " ++ body ++ "\n}"
    where   args = intercalate ", " $ map genTsym tsyms
            body = genReturn expr

genReturn :: Expr -> [Char]
genReturn expr = "return " ++ genRawExpr expr ++ ";"

genRawExpr :: Expr -> [Char]
genRawExpr (Func expr exprs) = genExpr (Func expr exprs)
genRawExpr (BinaryOp op expr1 expr2) = genExpr (BinaryOp op expr1 expr2)
genRawExpr (UnaryOp op expr) = genExpr (UnaryOp op expr)
genRawExpr (Lambda tsyms t expr) = genExpr (Lambda tsyms t expr)
genRawExpr e = genExpr e ++ "()"


toLambda :: [Char] -> [Char]
toLambda e = "[&]() { return " ++ e ++ "; }"
