module Generators2 where

import {-# SOURCE #-} Generators
import AST
import ExprConverter

import Data.List


genExpr :: Expr -> [Char]

genExpr (Literal val ('K':_)) = toLambda $ show (val * 1.0e3)
genExpr (Literal val ('M':_)) = toLambda $ show (val * 1.0e6)
genExpr (Literal val ('m':_)) = toLambda $ show (val * 1.0e-3)
genExpr (Literal val ('u':_)) = toLambda $ show (val * 1.0e-6)
genExpr (Literal val _) = toLambda $ show val

genExpr (Str s) = toLambda s

-- this is WRONG! not lazy. but should still compile.
genExpr (Attr iexpr (Symbol sym)) = "(" ++ genRawExpr expr ++ ")" ++ "." ++ sym 
    where expr = fst iexpr

genExpr (Tuple iexprs) = toLambda $ "std::make_tuple(" ++ es ++ ")"
    where es = intercalate ", " $ map (genExpr . fst) iexprs

genExpr (List iexprs) = "{" ++ intercalate ", " (map (genExpr . fst) iexprs) ++ "}"

genExpr (BinaryOp binOp ie1 ie2) = toLambda . genExpr $ binOpToFunc binOp ie1 ie2

genExpr (UnaryOp unOp iexpr) =
    toLambda . genExpr $ Func ((Var . Symbol . opToFunc $ unOp), -1) [iexpr]
    where opToFunc Negate = "psl::negate"

genExpr (Func iexpr iexprs) = toLambda $ "psl::apply(" ++ es ++ ")"
    where es = intercalate ", " $ map (genExpr . fst) (iexpr:iexprs)

genExpr (Var (Symbol sym))
    | sym == "sin"          = "psl::sin"
    | sym == "cos"          = "psl::cos"
    | sym == "ft"           = "psl::ft"
    | sym == "intervalMap"  = "psl::intervalMap"
    | otherwise     = sym

genExpr (Lambda tsyms _ iexpr) = toLambda $ genLambda tsyms expr
    where expr = fst iexpr

genExpr (LetExp ds iexpr) = "[&]() {" ++ n:decs ++ n:defs ++ n:out ++ n:"}"
    where   (decs, defs) = genDefs ds
            out = genReturn expr
            n = '\n'
            expr = fst iexpr

genExpr (Cond iexpr1 iexpr2 iexpr3) = "[&]() {\n" ++ cond ++ "\n}"
    where   cond = "if(" ++ e1 ++ ") {\n" ++ e2 ++ "\n}\nelse {\n" ++ e3 ++ "\n};"
            e1 = genRawExpr $ fst iexpr1
            e2 = genReturn $ fst iexpr2
            e3 = genReturn $ fst iexpr3


genLambda :: Tsyms -> Expr -> [Char]
genLambda tsyms expr = "[&](" ++ args ++ ") {\n " ++ body ++ "\n}"
    where   args = intercalate ", " $ map genTsym tsyms
            body = genReturn expr

genReturn :: Expr -> [Char]
genReturn expr = "return " ++ genRawExpr expr ++ ";"

genRawExpr :: Expr -> [Char]
genRawExpr e = genExpr e ++ "()"


toLambda :: [Char] -> [Char]
toLambda e = "[&]() { return " ++ e ++ "; }"
