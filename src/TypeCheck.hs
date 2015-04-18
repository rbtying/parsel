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
                | Node  { index :: Int
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
typeCheck (Empty, sd, ast) = writer (ast, [Good])
typeCheck ((Node index scope children), sd, ast) = writer (ast, [Good])

--checkExpr :: VarTree -> StructData -> Expr -> Writer [Semantics] AST
--checkExpr tree sd (Literal _ _) = writer (ast, [Good])
--checkExpr tree sd (Attr expr sym) = writer (ast, [Good])

--getType :: Expr -> Type
--getType _ = Literal "" ""
