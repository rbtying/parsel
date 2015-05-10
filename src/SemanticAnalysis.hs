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
defCheck ast =
    do  varScope <- getScopes ast
        return (varScope, getStructs ast, ast)

getScopes :: AST -> Writer [Error] VarScope
getScopes ast =
    do  let topScope = Node builtInScope Empty
        scopes <- mapM (getTopDefScope topScope) ast
        return $ Map.unions scopes
        

getTopDefScope :: ScopeTree -> TopDef -> Writer [Error] VarScope
getTopDefScope parent (Def def) = getDefScope parent def
defTopDefScope _ _ = return Map.empty

getDefScope :: ScopeTree -> Def -> Writer [Error] VarScope
getDefScope parent (FuncDef sym tsyms rt expr) =
    do  let scope = Node table parent
            table = Map.insert sym ft $ toScopeTable tsyms
            ft = FuncType (map (\(Tsym t _) -> t) tsyms) rt
        scope <- getExprScope scope expr
        return scope 

getExprScope :: ScopeTree -> Expr -> Writer [Error] VarScope
getExprScope scope (Var sym ind)
    | searchForDef scope sym = return table
    | otherwise = writer (table, [Undef sym])
    where   table = Map.singleton ind scope


getExprScope parent (LetExp defs expr) = 
    do  scopes <- mapM (getExprScope scope) exprs 
        return $ Map.unions scopes
        where   scope = Node table parent
                table = Map.fromList $ map toPair defs
                toPair (VarDef (Tsym t sym) _) = (sym, t)
                toPair (FuncDef sym tsyms rt _) = (sym, ft)
                    where ft = FuncType (map (\(Tsym t _) -> t) tsyms) rt
                exprs = expr:(map getExpr defs)
                getExpr (VarDef _ e) = e
                getExpr (FuncDef _ _ _ e) = e
getExprScope parent (Lambda tsyms rt expr) = getExprScope scope expr
    where   scope = Node table parent
            table = toScopeTable tsyms
getExprScope scope (Cond expr1 expr2 expr3) =
    do  let exprs = [expr1, expr2, expr3]
        scopes <- mapM (getExprScope scope) exprs
        return $ Map.unions scopes
getExprScope scope (BinaryOp binop expr1 expr2) =
    do  let exprs = [expr1, expr2]
        scopes <- mapM (getExprScope scope) exprs
        return $ Map.unions scopes
getExprScope scope (Attr expr _) = getExprScope scope expr
getExprScope scope (Tuple exprs) =
    do  scopes <- mapM (getExprScope scope) exprs
        return $ Map.unions scopes
getExprScope scope (List exprs) =
    do  scopes <- mapM (getExprScope scope) exprs
        return $ Map.unions scopes
getExprScope scope (UnaryOp unop expr) = getExprScope scope expr
getExprScope _ _ = return Map.empty

searchForDef :: ScopeTree -> Symbol -> Bool
searchForDef (Node scope parent) sym
    | Map.member sym scope = True
    | otherwise = searchForDef parent sym
searchForDef Empty _ = False

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
