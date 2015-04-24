module SemanticAnalysis where

import AST
import TypeCheck

import Control.Monad.Writer

import qualified Data.Map as Map


semAnalysis :: AST -> Writer [Semantics] AST
semAnalysis ast = defCheck ast >>= typeCheck >>= mainCheck

mainCheck :: AST -> Writer [Semantics] AST
mainCheck ast
    | (length (filter findGoodMain ast)) == 1 = writer(ast, [Good])
    | otherwise = writer(ast, [NoMain])

findGoodMain :: TopDef -> Bool
findGoodMain (Def d) = findGoodMainDef d 
findGoodMain (Struct (Symbol sym) tsyms) = False

findGoodMainDef :: Def -> Bool
findGoodMainDef (FuncDef (Symbol sym) tsyms rt expr)
	| sym == "main" = if length ts > 0 then True else False
	| otherwise = False
	 where TupleType ts = rt
findGoodMainDef (VarDef tsym expr) = False

defCheck :: AST -> Writer [Semantics] (ExprScope, StructData, AST)
defCheck ast = writer ((Map.empty, Map.empty, ast), [Good]) 
