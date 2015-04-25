module SemanticAnalysis where

import AST
import TypeCheck

import Control.Monad.Writer

import qualified Data.Map as Map


semAnalysis :: AST -> Writer [Error] AST
semAnalysis ast = defCheck ast >>= typeCheck >>= mainCheck

mainCheck :: AST -> Writer [Error] AST
mainCheck ast
    | (length (filter findGoodMain ast)) == 1 = return ast
    | otherwise = writer (ast, [NoMain])

findGoodMain :: TopDef -> Bool
findGoodMain (Def d) = findGoodMainDef d 
findGoodMain (Struct (Symbol sym) tsyms) = False

findGoodMainDef :: Def -> Bool
findGoodMainDef (FuncDef (Symbol sym) tsyms rt expr)
	| sym == "main" = if length ts > 0 then True else False
	| otherwise = False
	 where TupleType ts = rt
findGoodMainDef (VarDef tsym expr) = False

defCheck :: AST -> Writer [Error] (ExprScope, StructData, AST)
defCheck ast = return (Map.empty, Map.empty, ast)
