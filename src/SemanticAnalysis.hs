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
findGoodMain (Def d) = isGoodMain d 
    where   isGoodMain (FuncDef (Symbol sym) tsyms (TupleType ts) _) =
                goodArgs && length ts > 1 && sym == "main"
                where   goodArgs = length (filter isCharList tsyms) == 0

                        isCharList (Tsym t _) = t == cltype
                            where cltype = ListType . Type . Symbol $ "char"

            isGoodMain (FuncDef _ _ _ _) = False
            isGoodMain _ = False
findGoodMain (Struct _ _) = False

defCheck :: AST -> Writer [Error] (VarScope, StructData, AST)
defCheck ast = return (Map.empty, Map.empty, ast)
