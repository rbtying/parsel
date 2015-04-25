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
type ExprScope = Map.Map Int VarTree

data VarTree    = Empty
                | Node  { treeIndex :: Int
                        , treeScope :: VarTable
                        , treeChildren :: [VarTree]
                        , treeParent :: VarTree
                        }

type VarTable = Map.Map Symbol VarData

data VarData = VarData  { varType :: Type
                        , varLine :: Int
                        , varDef :: Def
                        }

type StructData = Map.Map Type (Map.Map Symbol Type)

 
typeCheck :: (ExprScope, StructData, AST) -> Writer [Error] AST
typeCheck (es, sd, ast) = mapM (checkTopDef es sd) ast

checkTopDef :: ExprScope -> StructData -> TopDef -> Writer [Error] TopDef
checkTopDef es sd (Def d) = checkDef es sd d >>= return . Def
checkTopDef _ _ (Struct sym tsyms) = return $ Struct sym tsyms

checkDef :: ExprScope -> StructData -> Def -> Writer [Error] Def
checkDef es sd (FuncDef sym tsyms t iexpr) =
    do  iexpr' <- checkExpr es sd iexpr
        iexpr'' <- attemptCast es sd t iexpr'
        return $ FuncDef sym tsyms t iexpr''
checkDef es sd (VarDef (Tsym t sym) iexpr) =
    do  iexpr' <- checkExpr es sd iexpr
        iexpr'' <- attemptCast es sd t iexpr'
        return $ VarDef (Tsym t sym) iexpr''


checkExpr :: ExprScope -> StructData -> IndExpr -> Writer [Error] IndExpr
checkExpr es sd (Attr iexpr sym, _) = checkExpr es sd iexpr
checkExpr es sd (Tuple iexprs, i) = mapM (checkExpr es sd) iexprs >>= newTuple 
    where newTuple ies = return (Tuple ies, i)
checkExpr es sd (List iexprs, i)
    | length iexprs > 0 = 
        do  let tM = getType es sd $ head iexprs
                checkAndCast (Right t) e = checkExpr es sd e >>= attemptCast es sd t
                checkAndCast (Left err) e = writer (e, [MisusedType err])
                newList ies = return (List ies, i)
            mapM (checkAndCast tM) iexprs >>= newList
    | otherwise = return list
    where list = (List iexprs, i)
-- checkExpr es sd (BinaryOp op ie1 ie2, i)
checkExpr es sd iexpr = return iexpr



attemptCast :: ExprScope -> StructData -> Type -> IndExpr -> Writer [Error] IndExpr
attemptCast es sd t iexpr = attemptCast' $ getType es sd iexpr
    where   attemptCast' (Right et)
                | et == t =
                    return iexpr
                | et == TupleType [t] =
                    return $ toTuple iexpr
                | TupleType [et] == t =
                    return $ fromTuple iexpr
                | otherwise =
                    writer (iexpr, [WrongType t et])
            attemptCast' (Left err) = writer (iexpr, [MisusedType err])



getType :: ExprScope -> StructData -> IndExpr -> Either String Type
getType _ _ (Literal _ unit, _)
    | unit `endsWith` "s"   = toRightType "time"
    | unit `endsWith` "Hz"  = toRightType "freq"
    | otherwise             = Left "not a unit"
    where toRightType = Right . Type . Symbol
getType es sd (Attr iexpr sym, _) =
    do  exprType <- getType es sd iexpr
        members <- toEither "not a type" $ Map.lookup exprType sd
        memberType <- toEither "not a member" $ Map.lookup sym members
        return memberType
getType es sd (Tuple iexprs, _) = 
    do  types <- mapM (getType es sd) iexprs
        return $ TupleType types
getType es sd (List iexprs, _) = 
    do  types <- mapM (getType es sd) iexprs
        return . ListType . head $ types
getType es sd (BinaryOp op ie1 ie2, i) = getType es sd $ (binOpToFunc op ie1 ie2, i)
getType es sd (UnaryOp _ ie, _) = getType es sd ie
getType es sd (Func ie _, _) =
    do  ftype <- getType es sd ie

        let returnType (FuncType _ t) = Right t
            returnType _ = Left "not a function"

        returnType ftype
getType es _ (Var sym, i) =
    do  varData <- toEither "sym not found" $ searchForSym es sym i
        return $ varType varData
getType _ _ (Lambda _ t _, _) = Right t
getType es sd (LetExp _ ie, _) = getType es sd ie
getType es sd (Cond _ ie _, _) = getType es sd ie
getType _ _ (Str _, _) = Right . ListType . Type . Symbol $ "char"
 
toEither :: [Char] -> Maybe a -> Either [Char] a
toEither _ (Just t) = Right t
toEither s Nothing = Left s

searchForSym :: ExprScope -> Symbol -> Int -> Maybe VarData
searchForSym es sym i = Map.lookup i es >>= searchUp
    where   searchUp Empty = Nothing
            searchUp (Node _ scope _ parent) =
                let maybeData = Map.lookup sym scope
                    keepSearching (Just varData) = Just varData
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

toTuple :: IndExpr -> IndExpr
toTuple (expr, i) = (Tuple [iexpr], i)
    where iexpr = (expr, i)

fromTuple :: IndExpr -> IndExpr
fromTuple (Tuple [iexpr], _) = iexpr
fromTuple iexpr = iexpr
