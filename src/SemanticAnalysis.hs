module SemanticAnalysis where

import AST
import TypeCheck
import GlobalScope

import Control.Monad.Writer
import Data.Maybe
import qualified Data.Map as Map


semAnalysis :: AST -> Writer [Error] AST
semAnalysis ast = defCheck (ast, getStructs ast) >>= mainCheck

mainCheck :: AST -> Writer [Error] AST
mainCheck ast
    | any isJust mains = return ast'
    | otherwise = writer (ast, [NoMain])
    where   mains = map toGoodMain ast
            ast' = map fromM $ zip mains ast
            
            fromM (Just td', td) = td'
            fromM (Nothing, td) = td


toGoodMain :: TopDef -> Maybe TopDef
toGoodMain (Def (FuncDef (Symbol sym) tsyms (TupleType ts) expr))
    | isGoodMain = Just $ Def (FuncDef (Symbol sym) tsyms (TupleType ts) expr')
    | otherwise = Nothing
    where   expr' = List exprs
            Tuple exprs = expr
            
            isGoodMain  = goodArgs && goodts && length ts > 0 && sym == "main"
            goodArgs = all isCharList tsyms
            goodts = all isSignal ts

            isCharList (Tsym t _) = t == cltype
                where cltype = ListType . Type . Symbol $ "char"
            isSignal t = t == sigtype
                where sigtype = Type . Symbol $ "signal"
toGoodMain _ = Nothing

getStructs :: AST -> StructData
getStructs ast = Map.fromList (map getStruct [1..numberStructs])
    where   getStruct n = (getStructType n ast, getStructSymbols n ast)
            numberStructs = length (structList ast)

structList :: AST -> [TopDef]
structList ast = filter getListStructs ast
    where   getListStructs (Struct (Symbol _) _) = True
            getListStructs (Def _) = False

getStructType :: Int -> AST -> Type
getStructType n ast = structType ((structList ast) !! (n-1))
    where   structType (Struct (Symbol sym) _) = Type (Symbol sym)
            structType (Def _) = Type (Symbol "")

getStructSymbols :: Int -> AST -> (Map.Map Symbol Type)
getStructSymbols n ast = Map.fromList (map getSymbolMap [1..numberSymbols])
    where   getSymbolMap n2 = (getStructSymbol n n2 ast, getSymType n n2 ast)
            numberSymbols = (symbolLength n ast)

getStructSymbol :: Int -> Int -> AST -> Symbol
getStructSymbol n n2 ast = getTsym ((structList ast) !! (n-1))
    where   getTsym (Struct (Symbol _) ts) =  getSymbol (ts !! n2)
              where   getSymbol (Tsym (Type _) (Symbol s)) = (Symbol s) 
                      getSymbol (Tsym (ListType _) _) = Symbol ""
                      getSymbol (Tsym (TupleType _) _) = Symbol ""
                      getSymbol (Tsym (FuncType _ _) _) = Symbol ""
            getTsym (Def _) = Symbol ""

getSymType :: Int -> Int -> AST -> Type
getSymType n n2 ast = getTsym ((structList ast) !! (n-1))
    where   getTsym (Struct (Symbol _) ts) = getTsymType (ts !! n2)
             where  getTsymType (Tsym  (Type t) (Symbol _)) = (Type t)
                    getTsymType (Tsym (ListType _) _) = Type (Symbol "")
                    getTsymType (Tsym (TupleType _) _) = Type (Symbol "")
                    getTsymType (Tsym (FuncType _ _) _) = Type (Symbol "")
            getTsym (Def _) = Type (Symbol "")
symbolLength :: Int -> AST -> Int
symbolLength n ast = lengthTsym ((structList ast) !! (n-1))
    where   lengthTsym (Struct (Symbol _) ts) = length ts
            lengthTsym (Def _) = 0
