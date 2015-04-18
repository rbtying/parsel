module SemanticAnalysis where

import AST
import TypeCheck

import Control.Monad.Writer


semAnalysis :: AST -> Writer [Semantics] AST
semAnalysis ast = defCheck ast >>= typeCheck >>= mainCheck

mainCheck :: AST -> Writer [Semantics] AST
mainCheck ast = writer (ast, [Good])

defCheck :: AST -> Writer [Semantics] (VarTree, AST)
defCheck ast = writer ((Empty, ast), [Good]) 
