module Generators2 where

import {-# SOURCE #-} Generators
import Data.List
import AST

genExpr :: Expr -> [Char]

-- Q: how are these types expressed in C ? (time literal?)
-- guard for each unit type? 
-- is this rhs of a definition?
genExpr (Literal val unit) = show val

genExpr (Attr expr (Symbol sym)) = genExpr expr ++ "." ++ sym 

genExpr (Tuple exprs) = "std::make_tuple(" ++ es ++ ")"
    where es = intercalate ", " $ map genExpr exprs

genExpr (List exprs) = "{" ++ intercalate ", " (map genExpr exprs) ++ "}"

-- waiting on fill functions
genExpr (BinaryOp binOp expr1 expr2) = "(binaryOp binOp expr1 expr2)"

genExpr (UnaryOp unOp expr) = "(unaryop unop expr)"

genExpr (Func expr exprs) = genExpr expr ++ "(" ++ es ++ ")"
    where es = intercalate ", " $ map ((++"()") . genExpr) exprs
    

genExpr (Var (Symbol sym)) = sym

genExpr (Lambda tsyms t expr) = "lambda tsyms t expr"

genExpr (LetExp ds expr) = "[&]() {" ++ n:decs ++ n:defs ++ n:out ++ n:"}()"
    where   (decs, defs) = genDefs ds
            out = "return " ++ genExpr expr ++ ";"
            n = '\n'

genExpr (Cond expr1 expr2 expr3) = "cond expr1 expr2 expr3"
