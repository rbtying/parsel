module TypeCheck where

import AST
import GlobalScope
import ExprConverter

import Control.Monad.Writer
import Data.Maybe
import Data.List
import Data.Either
import Data.Function
import qualified Data.Map as Map

data Error  = WrongType [Type] [Type]
                | MisusedType [Char]
                | Undef Symbol
                | Multdef Symbol
                | NoMain
                | BadStructAttr
                deriving (Show, Eq)

data ScopeStack = Empty
                | Scope SymbolTable ScopeStack

defCheck :: (AST, StructData) -> Writer [Error] AST
defCheck (ast, sd) = mapM (checkTopDef sd topScope) ast
    where   topScope = Scope table $ Scope builtInScope Empty
            table = defsToTable . map toDef . filter isDef $ ast
                where   isDef (Def _) = True
                        isDef _ = False
                        toDef td = let Def d = td in d


checkTopDef :: StructData -> ScopeStack -> TopDef -> Writer [Error] TopDef
checkTopDef sd scope (Def def) =
    do  def' <- checkDef sd scope def
        return $ Def def'
checkTopDef _ _ td = return td


checkDef :: StructData -> ScopeStack -> Def -> Writer [Error] Def
checkDef sd parent (FuncDef sym tsyms rt expr) =
    do  let scope = Scope table parent
            table = Map.insert sym [ft] $ toScopeTable tsyms
            ft = FuncType (map (\(Tsym t _) -> t) tsyms) rt

            rt' = if s == "main" then ListType . Type . Symbol $ "signal" else rt
            Symbol s = sym
        expr' <- checkExpr sd scope expr
        expr'' <- attemptCast sd scope [rt'] expr'
        return $ FuncDef sym tsyms rt expr''
checkDef sd scope (VarDef (Tsym t sym) expr) =
    do  expr' <- checkExpr sd scope expr
        expr'' <- attemptCast sd scope [t] expr'
        return $ VarDef (Tsym t sym) expr''


checkExpr :: StructData -> ScopeStack -> Expr -> Writer [Error] Expr
checkExpr sd scope (Attr expr _) = checkExpr sd scope expr
checkExpr sd scope (Tuple exprs) =
    do  exprs' <- mapM (checkExpr sd scope) exprs
        return $ Tuple exprs'
checkExpr sd scope (List exprs)
    | length exprs > 0 = 
        do  let ts = getTypes sd scope $ head exprs
            exprs' <- mapM (checkAndCast sd scope ts) exprs
            return $ List exprs'
    | otherwise = return $ List exprs
checkExpr sd scope (BinaryOp op e1 e2) = checkExpr sd scope $ binOpToFunc op e1 e2
checkExpr sd scope (UnaryOp op expr) = checkExpr sd scope $ unOpToFunc op expr
checkExpr sd scope (Func expr exprs) =
    do  expr' <- checkExpr sd scope expr

        let toArgTypes (Right ts) = mapM toArgs ts
                where   toArgs (FuncType ts' _) = return $ map (\t -> [t]) ts'
                        toArgs _ = writer ([], [MisusedType "Not a function"])
            toArgTypes (Left err) = writer ([], [MisusedType err])
            types = getTypes sd scope expr'

        argTypeLists <- toArgTypes types

        let checkList argTypes = foldM foldChecker [] $ zip checkers exprs
                where checkers = map (checkAndCast sd scope . Right) argTypes
                      foldChecker es (checker, e) = checker e >>= return . (:es)
            writerLength = length . snd . runWriter

            checkedLists = map checkList argTypeLists

        exprs' <-   if length argTypeLists > 0
                    then minimumBy (compare `on` writerLength) checkedLists
                    else mapM (checkExpr sd scope) exprs

        return $ Func expr' exprs'
checkExpr _ scope (Var sym i)
    | isJust $ searchForDef scope sym = return $ Var sym i
    | otherwise = writer (Var sym i, [Undef sym])
checkExpr sd parent (Lambda tsyms rt expr) =
    do  let scope = Scope (toScopeTable tsyms) parent
        expr' <- checkAndCast sd scope (return [rt]) expr
        return $ Lambda tsyms rt expr'
checkExpr sd parent (LetExp defs expr) =
    do  let scope = Scope (defsToTable defs) parent
        defs' <- mapM (checkDef sd scope) defs
        expr' <- checkExpr sd scope expr
        return $ LetExp defs' expr'
checkExpr sd scope (Cond e1 e2 e3) =
    do  let ts = getTypes sd scope (Cond e1 e2 e3)
            bool = return [Type $ Symbol "bool"]
        e1' <- checkAndCast sd scope bool e1
        e2' <- checkAndCast sd scope ts e2
        e3' <- checkAndCast sd scope ts e3
        return $ Cond e1' e2' e3'
checkExpr _ _ expr = return expr


defsToTable :: [Def] -> SymbolTable
defsToTable defs = Map.fromList $ map toPair defs
    where   toPair (VarDef (Tsym t sym) _) = (sym, [t])
            toPair (FuncDef sym tsyms rt _) = (sym, [ft])
                where   ft = FuncType ts rt
                        ts = map (\(Tsym t _) -> t) tsyms


toScopeTable :: [Tsym] -> SymbolTable
toScopeTable = Map.fromList . map (\(Tsym t s) -> (s, [t]))


checkAndCast :: StructData -> ScopeStack -> Either String [Type] -> Expr ->
                Writer [Error] Expr
checkAndCast sd scope (Right ts) expr =
    do  expr' <- checkExpr sd scope expr
        attemptCast sd scope ts expr'
checkAndCast _ _ (Left err) expr = writer (expr, [MisusedType err])


attemptCast :: StructData -> ScopeStack -> [Type] -> Expr -> Writer [Error] Expr
attemptCast sd scope ts expr = toWriter $ attemptCast' sd scope ts expr
    where   toWriter (Just expr') = return expr'
            toWriter Nothing = writer (expr, [WrongType ts ets])
            ets = toTypes . getTypes sd scope $ expr
            toTypes (Right ets') = ets'
            toTypes (Left _) = []

attemptCast' :: StructData -> ScopeStack -> [Type] -> Expr -> Maybe Expr
attemptCast' sd scope ts expr = toMaybeExpr $ getTypes sd scope expr
    where toMaybeExpr (Right ets) = if ets == [] || ts == []
                                    then Just expr
                                    else if length casts > 0
                                    then head casts
                                    else Nothing
            where   casts = filter isJust $ map cast typePairs
                    cast (t, et) = castType sd scope expr t et
                    typePairs = [(t, et) | t <- ts, et <- ets]
          toMaybeExpr (Left _) = Nothing


castType :: StructData -> ScopeStack -> Expr -> Type -> Type -> Maybe Expr
castType sd scope (Tuple exprs) (TupleType ts) _ =
    mapM cast (zip ts exprs) >>= Just . Tuple
    where   cast (t, expr) = attemptCast' sd scope [t] expr
castType sd scope (List exprs) (ListType t) _ =
    mapM (attemptCast' sd scope [t]) exprs >>= Just . List
castType _ _ expr t et
    | et == t =
        Just expr
    | et == TupleType [t] =
        Just $ fromTuple expr
    | TupleType [et] == t =
        Just $ Tuple [expr]
    | t == complex && (et == float || et == int) =
        Just $ Func (Var (Symbol "toComplex") 0) [expr]
    | et == complex && (t == float || t == int) =
        Just $ Func (Var (Symbol "toComplex") 0) [expr]
    | otherwise =
        Nothing
    where   complex = Type . Symbol $ "complex"
            float   = Type . Symbol $ "float"
            int     = Type . Symbol $ "int"


getTypes :: StructData -> ScopeStack -> Expr -> Either String [Type]
getTypes _ _ (Literal _ unit)
    | unit == []            = toRightType "float"
    | unit `endsWith` "s"   = toRightType "time"
    | unit `endsWith` "Hz"  = toRightType "freq"
    | otherwise             = Left $ "Not a unit: " ++ unit
    where toRightType s = Right [Type $ Symbol s]
getTypes sd scope (Attr expr sym) =
    do  exprTypes <- getTypes sd scope expr

        let getMembers [exprType] = toEither "Not a struct" $ Map.lookup exprType sd
            getMembers _ = Left "Ambiguous type"
        members <- getMembers exprTypes

        memberType <- toEither "Not a member" $ Map.lookup sym members
        return [memberType]
getTypes sd scope (Tuple exprs) = 
    do  types <- mapM (getTypes sd scope) exprs
        return $ map TupleType types
getTypes sd scope (List exprs) = 
    do  types <- mapM (getTypes sd scope) exprs
        return $ map (ListType . head) types
getTypes sd scope (BinaryOp op e1 e2) = getTypes sd scope $ (binOpToFunc op e1 e2)
getTypes sd scope (UnaryOp _ expr) = getTypes sd scope expr
getTypes sd scope (Func expr _) =
    do  ftype <- getTypes sd scope expr

        let returnType (FuncType _ rt) = Right rt
            returnType _ = err
            err = Left $ "Not a function: " ++ show expr

        mapM returnType ftype
getTypes _ scope (Var sym _) = toEither err $ searchForDef scope sym
  where   Symbol s = sym
          err = "Symbol not found: " ++ s
getTypes _ _ (Lambda tss rt _) = Right $ [FuncType ts rt]
    where ts = map (\(Tsym t _) -> t) tss
getTypes sd scope (LetExp _ e) = getTypes sd scope e
getTypes sd scope (Cond _ _ e) = getTypes sd scope e
getTypes _ _ (Str _) = Right [ListType . Type . Symbol $ "char"]


searchForDef :: ScopeStack -> Symbol -> Maybe [Type]
searchForDef (Scope table parent) sym
    | isNothing tM = searchForDef parent sym
    | otherwise = tM
    where tM = Map.lookup sym table
searchForDef Empty _ = Nothing

 
toEither :: [Char] -> Maybe a -> Either [Char] a
toEither _ (Just t) = Right t
toEither s Nothing = Left s


isNumber :: Type -> Bool
isNumber (Type (Symbol sym))
    | sym == "float"    = True
    | sym == "int"      = True
    | sym == "complex"  = True
    | otherwise         = False
isNumber _ = False


endsWith :: [Char] -> [Char] -> Bool
endsWith str suf = not $ False `elem` zipWith (==) (reverse str) (reverse suf)


fromTuple :: Expr -> Expr
fromTuple (Tuple [expr]) = expr
fromTuple expr = expr
