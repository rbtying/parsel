module SemanticAnalysis where

import AST
import TypeCheck

import Control.Monad.Writer

import qualified Data.Map as Map


semAnalysis :: AST -> Writer [Semantics] AST
semAnalysis ast = defCheck ast >>= typeCheck >>= mainCheck

mainCheck :: AST -> Writer [Semantics] AST
mainCheck ast = writer (ast, [Good])

defCheck :: AST -> Writer [Semantics] (VarTree, StructData, AST)
defCheck ast = writer ((Empty, Map.empty, ast), [Good]) 
