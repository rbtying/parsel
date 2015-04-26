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
                goodArgs && goodts && length ts > 0 && sym == "main"
                where   goodArgs = length (filter isNotCharList tsyms) == 0
                        goodts = length (filter isNotSignal ts) == 0

                        isNotCharList (Tsym t _) = t /= cltype
                            where cltype = ListType . Type . Symbol $ "char"

                        isNotSignal (Type (Symbol s)) = s == "psl::signal"
                        isNotSignal _ = False

            isGoodMain (FuncDef _ _ _ _) = False
            isGoodMain _ = False
findGoodMain (Struct _ _) = False

defCheck :: AST -> Writer [Error] (VarScope, StructData, AST)
defCheck ast = return (Map.empty, getStructs ast, ast)

getStructs :: AST -> StructData
getStructs ast = Map.fromList (map getStruct [1..numberStructs])
    where   getStruct n = (getType n ast, getSymbols n)
            numberStructs = length (structList ast)

structList :: AST -> [Struct]
structList ast = filter getListStructs ast

getListStructs :: TopDef -> Bool
getListStructs (Struct (Symbol sym) ts) = 

getType :: Int -> AST -> Symbol
getType n ast = structType ((structList ast) !! (n-1))

structType :: Struct -> Bool
structType (Struct (Symbol sym) _) = True
structType (Def d) = False
