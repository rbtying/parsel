module TypeCheck where

import AST

import Control.Monad.Writer
import qualified Data.Map as Map

data Semantics  = Good
                | TypeError Type Type
                | Undef Symbol
                | Multdef Symbol
                | NoMain
                deriving (Show, Eq)


data VarTree    = Empty
                | Node  { index :: Int
                        , scope :: VarTable
                        , children :: [VarTree]
                        }


type VarTable = Map.Map Symbol VarData

data VarData = VarData  { varType :: Type
                        , line :: Int
                        }

typeCheck :: (VarTree, AST) -> Writer [Semantics] AST
typeCheck (table, ast) = writer (ast, [Good])
