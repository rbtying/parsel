module TypeCheck where

import AST

import Control.Monad.Writer
import qualified Data.Map as Map

data Semantics  = Good
                | TypeError Type Type
                | Undef Symbol
                | Multdef Symbol
                | NoMain
                | BadStructAttr
                deriving (Show, Eq)

data VarTree    = Empty
                | Node  {index :: Int
                        , scope :: VarTable
                        , children :: [VarTree]
                        }

type VarTable = Map.Map Symbol VarData

data VarData = VarData  { varType :: Type
                        , line :: Int
                        , def :: Def
                        }

type StructData = Map.Map Type (Map.Map Symbol Type)

 
typeCheck :: (VarTree, StructData, AST) -> Writer [Semantics] AST
typeCheck (vt, sd, ast) = writer (ast, [Good])
--typeCheck (vt, sd, ast) = mapM (checkTopDef vt sd) ast
--
--checkTopDef :: VarTree -> StructData -> TopDef -> Writer [Semantics] TopDef
--checkTopDef vt sd (Def d) = checkDef vt sd d >>= return . Def
--checkTopDef vt sd (Struc sym tsyms) = writer ((Struct sym tsyms), [Good])
--
--checkDef :: VarTree -> StructData -> Def -> Writer [Semantics] Def
--checkDef vt sd (FuncDef sym tsyms t expr)
--    | exprType == t = exprCheck >>= return . exprToDef
--    | exprType == TupleType [t] = exprCheck >>= return . exprToDef . toTuple
--    | TupleType [exprType] == t = exprCheck >>= return . exprToDef . fromTuple
--    | otherwise = exprCheck >>= writer (def, [TypeError t exprType])
--    where   (exprType, newExpr) = getType vt sd expr
--            exprCheck = checkExpr vt sd newExpr
--            exprToDef = FuncDef sym tsyms t
--            def = exprToDef expr
--
--checkExpr :: VarTree -> StructData -> Expr -> Writer [Semantics] Expr
--checkExpr vt sd expr = writer (expr, [Good])
--
---- Finish getType
--
--getType :: VarTree -> StructData -> Expr -> (Type, Expr)
--getType vt sd (Literal val unit)
--    | unit `endsWith` "s"  = (Type $ Symbol "time", Literal val unit)
--    | unit `endsWith` "Hz"  = (Type $ Symbol "freq", Literal val unit)
--getType vt sd 
--
--
--endsWith :: [Char] -> [Char] -> Bool
--endsWith str suf = not $ False `elem` zipWith (==) (reverse str) (reverse suf)
--
--toTuple :: Expr -> Expr
--toTuple expr = Tuple [expr]
--
--fromTuple :: Expr -> Expr
--fromTuple Tuple [expr] = expr
--fromTuple e = e
