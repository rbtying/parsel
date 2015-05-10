module SemanticAnalysis where

import AST
import TypeCheck
import GlobalScope

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

                        isNotSignal (Type (Symbol s)) = s /= "psl::signal"
                        isNotSignal _ = True

            isGoodMain (FuncDef _ _ _ _) = False
            isGoodMain _ = False
findGoodMain (Struct _ _) = False

defCheck :: AST -> Writer [Error] (VarScope, StructData, AST)
defCheck ast = return (getScopes ast, getStructs ast, ast)

getScopes :: AST -> VarScope
getScopes ast = Map.unions $ map (getTopDefScope topScope) ast
    where   topScope = Node builtInScope Empty

getTopDefScope :: ScopeTree -> TopDef -> VarScope
getTopDefScope parent (Def def) = getDefScope parent def 
getTopDefScope _ _ = Map.empty

getDefScope :: ScopeTree -> Def -> VarScope
getDefScope parent (FuncDef sym tsyms rt expr) 
    = getExprScope scope expr
    where   scope = Node (Map.insert sym t $ toScopeTable tsyms) parent
            t = FuncType (map (\(Tsym t' _) -> t') tsyms) rt
getDefScope parent (VarDef tsym expr) = getExprScope parent expr


getExprScope :: ScopeTree -> Expr -> VarScope
getExprScope scope (Var _ ind) = Map.singleton ind scope
getExprScope _ _ = Map.empty


toScopeTable :: [Tsym] -> SymbolTable
toScopeTable = Map.fromList . map (\(Tsym t s) -> (s, t))


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
 
getSymType :: Int -> Int -> AST -> Type
getSymType n n2 ast = getTsym ((structList ast) !! (n-1))
    where   getTsym (Struct (Symbol _) ts) = getTsymType (ts !! n2)
             where  getTsymType (Tsym  (Type t) (Symbol _)) = (Type t)

symbolLength :: Int -> AST -> Int
symbolLength n ast = lengthTsym ((structList ast) !! (n-1))
    where   lengthTsym (Struct (Symbol _) ts) = length ts
