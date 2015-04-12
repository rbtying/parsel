module Generators2 where
import {-# SOURCE #-} Generators()
import Data.List
import AST
genExpr :: Expr -> [Char]

-- Q: how are these types expressed in C ? (time literal?)
-- guard for each unit type? 
-- is this rhs of a definition?
genExpr (Literal val unit) = show val

genExpr (Attr expr (Symbol sym)) = genExpr expr ++ "." ++ sym 
genExpr (Tuple es) = "(tuple es)"

genExpr (List es) = "(list es)"

genExpr (Index expr1 expr2) = "(index expr1 expr2)"

genExpr (BinaryOp binOp expr1 expr2) = "(binaryOp binOp expr1 expr2)"

genExpr (UnaryOp unOp expr) = "(unaryop unop expr)"

genExpr (Func expr es) = genExpr expr ++ exprs
    where exprs = intercalate " also " $ map eachExpr [1..numExprs]
          eachExpr n = genExpr (es !! (n-1))
          numExprs = length es
    

genExpr (Var (Symbol sym)) = sym

genExpr (Lambda tsyms t expr) = "lambda tsyms t expr"
-- same pattern as below
genExpr (LetExp ds expr) = "({" ++ "defs" ++ genExpr expr ++ "})"
{-
genExpr (LetExp ds expr) = "({" ++ defs ++ genExpr expr ++ "})"
    where defs = intercalate " " $ map eachDef [1..numDefs]
          eachDef n = genDef (ds !! (n-1))
          numDefs = length ds
-}
genExpr (Cond expr1 expr2 expr3) = "cond expr1 expr2 expr3"
