module Generators2 where

import {-# SOURCE #-} Generators
import AST
import ExprConverter

import Data.List


genExpr :: Expr -> [Char]

genExpr (Literal val ('K':_)) = toChunk $ show (val * 1.0e3)
genExpr (Literal val ('M':_)) = toChunk $ show (val * 1.0e6)
genExpr (Literal val ('m':_)) = toChunk $ show (val * 1.0e-3)
genExpr (Literal val ('u':_)) = toChunk $ show (val * 1.0e-6)
genExpr (Literal val _) = toChunk $ show val

genExpr (Str s) = toChunk $ "psl::fromString(" ++ s ++ ")"

-- TODO: this is not lazy
genExpr (Attr expr (Symbol sym)) = "(" ++ genRawExpr expr ++ ")" ++ "." ++ sym 

genExpr (Tuple exprs) = toChunk $ "std::make_tuple(" ++ es ++ ")"
    where es = intercalate ", " $ map genExpr exprs

genExpr (List exprs) = toChunk $ "psl::toVector({" ++ es ++  "})"
    where es = intercalate ", " $ map genExpr exprs

genExpr (BinaryOp binOp e1 e2) = genExpr $ binOpToFunc binOp e1 e2

genExpr (UnaryOp unOp expr) = genExpr $ unOpToFunc unOp expr

genExpr (Func expr exprs) = toChunk $ "psl::apply(" ++ es ++ ")"
    where es = intercalate ", " $ map genExpr (expr:exprs)

genExpr (Var (Symbol sym) _)
    | sym == "sin"          = "psl::sin"
    | sym == "cos"          = "psl::cos"
    | sym == "ft"           = "psl::ft"
    | sym == "intervalMap"  = "psl::intervalMap"
    | sym == "toSignal"     = "psl::toSignal"
    | otherwise     = sym

genExpr (Lambda tsyms _ expr) = toChunk $ genLambda tsyms expr

genExpr (LetExp ds expr) = "[&]() {" ++ n:decs ++ n:defs ++ n:out ++ n:"}"
    where   (decs, defs) = genDefs ds
            out = genReturn expr
            n = '\n'

-- TODO: is this lazy?
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
genRawExpr e = genExpr e ++ "()"


toChunk :: [Char] -> [Char]
toChunk e = "psl::toChunk([&]() {\nreturn " ++ e ++ ";\n})"
