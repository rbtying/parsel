module SemanticAnalysis where

import AST
import TypeCheck

import Control.Monad.Writer
import qualified Data.Map as Map


semAnalysis :: AST -> Writer [Semantics] AST
semAnalysis ast = defCheck ast >>= typeCheck >>= mainCheck

mainCheck :: AST -> Writer [Semantics] AST
mainCheck ast = writer (ast, [Good])

defCheck :: AST -> Writer [Semantics] (VarTable, AST)
defCheck ast = writer ((Map.empty, ast), [Good]) 
