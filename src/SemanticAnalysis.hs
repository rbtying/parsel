module SemanticAnalysis where

import AST
import TypeCheck

import Control.Monad.Writer

import qualified Data.Map as Map


semAnalysis :: AST -> Writer [Semantics] AST
semAnalysis ast = defCheck ast >>= typeCheck >>= mainCheck

mainCheck :: AST -> Writer [Semantics] AST
mainCheck ast = if (length (filter findGoodMain ast)) == 1 then writer (ast, [Good]) else writer (ast, [NoMain])

findGoodMain :: TopDef -> Bool
findGoodMain (Def d) = findGoodMainDef d 
findGoodMain (Struct (Symbol sym) tsyms) = False

findGoodMainDef :: Def -> Bool
findGoodMainDef (FuncDef (Symbol sym) tsyms rt expr)
	| sym == "main" = if length ts > 0 then True else False
	| otherwise = False
	 where TupleType ts = rt
findGoodMainDef (VarDef tsym expr) = False

defCheck :: AST -> Writer [Semantics] (VarTree, StructData, AST)
defCheck ast = writer ((Empty, Map.empty, ast), [Good]) 
