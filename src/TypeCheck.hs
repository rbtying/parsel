module TypeCheck where

import AST
import ExprConverter

import Control.Monad.Writer
import qualified Data.Map as Map

data Error  = WrongType Type Type
                | MisusedType [Char]
                | Undef Symbol
                | Multdef Symbol
                | NoMain
                | BadStructAttr
                deriving (Show, Eq)

-- TODO: make this  more efficient by mapping to ints which map to VarTree
--  (reduce # of VarTree)
type VarScope = Map.Map Int ScopeTree
data ScopeTree  = Empty
                | Node  { treeScope :: SymbolTable
                        , treeParent :: ScopeTree
                        }
type SymbolTable = Map.Map Symbol Type

type StructData = Map.Map Type (Map.Map Symbol Type)

typeCheck :: (VarScope, StructData, AST) -> Writer [Error] AST
typeCheck (vs, sd, ast) = mapM (checkTopDef vs sd) ast

checkTopDef :: VarScope -> StructData -> TopDef -> Writer [Error] TopDef
checkTopDef vs sd (Def d) = checkDef vs sd d >>= return . Def
checkTopDef _ _ (Struct sym tsyms) = return $ Struct sym tsyms

checkDef :: VarScope -> StructData -> Def -> Writer [Error] Def
checkDef vs sd (FuncDef sym tsyms t expr) =
    do  expr' <- checkExpr vs sd expr
        expr'' <- attemptCast vs sd t expr'
        return $ FuncDef sym tsyms t expr''
checkDef vs sd (VarDef (Tsym t sym) expr) =
    do  expr' <- checkExpr vs sd expr
        expr'' <- attemptCast vs sd t expr'
        return $ VarDef (Tsym t sym) expr''


checkExpr :: VarScope -> StructData -> Expr -> Writer [Error] Expr
checkExpr vs sd (Attr expr _) = checkExpr vs sd expr
checkExpr vs sd (Tuple exprs) = mapM (checkExpr vs sd) exprs >>= return . Tuple
checkExpr vs sd (List exprs)
    | length exprs > 0 = 
        do  let tM = getType vs sd $ head exprs
            exprs' <- mapM (checkAndCast vs sd tM) exprs
            return $ List exprs'
    | otherwise = return $ List exprs
checkExpr vs sd (BinaryOp op e1 e2) = checkExpr vs sd $ binOpToFunc op e1 e2
checkExpr vs sd (UnaryOp op expr) = checkExpr vs sd $ unOpToFunc op expr
checkExpr vs sd (Func expr exprs) =
    do  expr' <- checkExpr vs sd expr

        let toArgTypes (Right (FuncType ts _)) = return ts
            toArgTypes (Right _) = return []
            toArgTypes (Left err) = writer ([], [MisusedType err])
        argTypes <- toArgTypes $ getType vs sd expr'

        let checkers = map (checkAndCast vs sd . Right) argTypes
            foldChecker es (checker, e) = checker e >>= return . (:es)
        exprs' <- foldM foldChecker [] $ zip checkers exprs

        return $ Func expr' exprs'
checkExpr vs sd (Lambda _ rt expr) = checkAndCast vs sd (return rt) expr
checkExpr vs sd (LetExp defs expr) =
    do  defs' <- mapM (checkDef vs sd) defs
        expr' <- checkExpr vs sd expr
        return $ LetExp defs' expr'
checkExpr vs sd (Cond e1 e2 e3) =
    do  e1' <- checkAndCast vs sd bool e1
        e2' <- checkAndCast vs sd bool e2
        e3' <- checkExpr vs sd e3
        return $ Cond e1' e2' e3'
    where bool = return . Type . Symbol $ "bool"
checkExpr _ _ expr = return expr

checkAndCast :: VarScope -> StructData -> Either String Type -> Expr -> Writer [Error] Expr
checkAndCast vs sd (Right t) expr = checkExpr vs sd expr >>= attemptCast vs sd t
checkAndCast _ _ (Left err) expr = writer (expr, [MisusedType err])



attemptCast :: VarScope -> StructData -> Type -> Expr -> Writer [Error] Expr
attemptCast vs sd t expr = attemptCast' $ getType vs sd expr
    where   attemptCast' (Right et)
                | et == t =
                    return expr
                | et == TupleType [t] =
                    return $ Tuple [expr]
                | TupleType [et] == t =
                    return $ fromTuple expr
                | otherwise =
                    writer (expr, [WrongType t et])
            attemptCast' (Left err) = writer (expr, [MisusedType err])



getType :: VarScope -> StructData -> Expr -> Either String Type
getType _ _ (Literal _ unit)
    | unit `endsWith` "s"   = toRightType "time"
    | unit `endsWith` "Hz"  = toRightType "freq"
    | otherwise             = Left $ "Not a unit: " ++ unit
    where toRightType = Right . Type . Symbol
getType vs sd (Attr expr sym) =
    do  exprType <- getType vs sd expr
        members <- toEither "Not a struct" $ Map.lookup exprType sd
        memberType <- toEither "Not a member" $ Map.lookup sym members
        return memberType
getType vs sd (Tuple exprs) = 
    do  types <- mapM (getType vs sd) exprs
        return $ TupleType types
getType vs sd (List exprs) = 
    do  types <- mapM (getType vs sd) exprs
        return . ListType . head $ types
getType vs sd (BinaryOp op e1 e2) = getType vs sd $ (binOpToFunc op e1 e2)
getType vs sd (UnaryOp _ expr) = getType vs sd expr
getType vs sd (Func expr _) =
    do  ftype <- getType vs sd expr

        let returnType (FuncType _ t) = Right t
            returnType _ = Left $ "Not a function: " ++ show expr

        returnType ftype
getType vs _ (Var sym i) = toEither err $ searchForSym vs sym i
  where   Symbol s = sym
          err = "Symbol not found: " ++ s
getType _ _ (Lambda tss rt _) = Right $ FuncType ts rt
    where ts = map (\(Tsym t _) -> t) tss
getType vs sd (LetExp _ e) = getType vs sd e
getType vs sd (Cond _ _ e) = getType vs sd e
getType _ _ (Str _) = Right . ListType . Type . Symbol $ "char"
 
toEither :: [Char] -> Maybe a -> Either [Char] a
toEither _ (Just t) = Right t
toEither s Nothing = Left s

searchForSym :: VarScope -> Symbol -> Int -> Maybe Type
searchForSym vs sym i = Map.lookup i vs >>= searchUp
    where   searchUp Empty = Nothing
            searchUp (Node scope parent) =
                let maybeData = Map.lookup sym scope
                    keepSearching (Just t) = Just t
                    keepSearching Nothing = searchUp parent
                in keepSearching maybeData

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
