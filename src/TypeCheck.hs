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
                        , treeChildren :: [ScopeTree]
                        , treeParent :: ScopeTree
                        }
type SymbolTable = Map.Map Symbol Def

type StructData = Map.Map Type (Map.Map Symbol Type)

 
typeCheck :: (VarScope, StructData, AST) -> Writer [Error] AST
typeCheck (es, sd, ast) = mapM (checkTopDef es sd) ast

checkTopDef :: VarScope -> StructData -> TopDef -> Writer [Error] TopDef
checkTopDef es sd (Def d) = checkDef es sd d >>= return . Def
checkTopDef _ _ (Struct sym tsyms) = return $ Struct sym tsyms

checkDef :: VarScope -> StructData -> Def -> Writer [Error] Def
checkDef es sd (FuncDef sym tsyms t expr) =
    do  expr' <- checkExpr es sd expr
        expr'' <- attemptCast es sd t expr'
        return $ FuncDef sym tsyms t expr''
checkDef es sd (VarDef (Tsym t sym) expr) =
    do  expr' <- checkExpr es sd expr
        expr'' <- attemptCast es sd t expr'
        return $ VarDef (Tsym t sym) expr''


checkExpr :: VarScope -> StructData -> Expr -> Writer [Error] Expr
checkExpr es sd (Attr expr _) = checkExpr es sd expr
checkExpr es sd (Tuple exprs) = mapM (checkExpr es sd) exprs >>= return . Tuple
checkExpr es sd (List exprs)
    | length exprs > 0 = 
        do  let tM = getType es sd $ head exprs
                checkAndCast (Right t) e = checkExpr es sd e >>= attemptCast es sd t
                checkAndCast (Left err) e = writer (e, [MisusedType err])
            mapM (checkAndCast tM) exprs >>= return . List
    | otherwise = return $ List exprs
-- checkExpr es sd (BinaryOp op e1 e2)
checkExpr _ _ expr = return expr



attemptCast :: VarScope -> StructData -> Type -> Expr -> Writer [Error] Expr
attemptCast es sd t expr = attemptCast' $ getType es sd expr
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
    | otherwise             = Left "not a unit"
    where toRightType = Right . Type . Symbol
getType es sd (Attr expr sym) =
    do  exprType <- getType es sd expr
        members <- toEither "not a struct" $ Map.lookup exprType sd
        memberType <- toEither "not a member" $ Map.lookup sym members
        return memberType
getType es sd (Tuple exprs) = 
    do  types <- mapM (getType es sd) exprs
        return $ TupleType types
getType es sd (List exprs) = 
    do  types <- mapM (getType es sd) exprs
        return . ListType . head $ types
getType es sd (BinaryOp op e1 e2) = getType es sd $ (binOpToFunc op e1 e2)
getType es sd (UnaryOp _ e) = getType es sd e
getType es sd (Func e _) =
    do  ftype <- getType es sd e

        let returnType (FuncType _ t) = Right t
            returnType _ = Left "not a function"

        returnType ftype
getType es _ (Var sym i) =
    do  def  <- toEither "sym not found" $ searchForSym es sym i
        return $ defToType def
    where   defToType (VarDef (Tsym t _) _) = t
            defToType (FuncDef _ tsyms t _) = FuncType ts t
                where   ts = map (\(Tsym t' _) -> t') tsyms
getType _ _ (Lambda _ t _) = Right t
getType es sd (LetExp _ e) = getType es sd e
getType es sd (Cond _ e _) = getType es sd e
getType _ _ (Str _) = Right . ListType . Type . Symbol $ "char"
 
toEither :: [Char] -> Maybe a -> Either [Char] a
toEither _ (Just t) = Right t
toEither s Nothing = Left s

searchForSym :: VarScope -> Symbol -> Int -> Maybe Def
searchForSym es sym i = Map.lookup i es >>= searchUp
    where   searchUp Empty = Nothing
            searchUp (Node scope _ parent) =
                let maybeData = Map.lookup sym scope
                    keepSearching (Just def) = Just def
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
