module Generators where

import Data.List
import Data.Ord

import AST
import Generators2

genDefs :: [Def] -> ([Char], [Char], [Char])
genDefs [] = ("you", "done", "goofed")
genDefs [def] = genDef def
genDefs (def:defs) = (topdef ++ topdefs, code ++ codes, mainloop)
    where   (topdef, code, mlNew) = genDef def
            (topdefs, codes, mlOld) = genDefs defs
            mainloop = maximumBy (comparing length) [mlNew, mlOld]


-- TODO: add wrapper for caching argument-less function evaluation results
genDef :: Def -> ([Char], [Char], [Char])
genDef (FuncDef (Symbol sym) tsyms t expr)
    | sym == "main" = 
        let mainloop = "bool B=true;\nwhile(" ++ conds ++ ") B = !B;\n"
            conds = intercalate "&&" $ map cond [1..numSigs]
            cond n = "out_.get(" ++ show (n-1) ++ ").fillBuffer(B)" 
            numSigs = length ts
            TupleType ts = t

            (topdef, code, _) = genDef (VarDef (Tsym t $ Symbol "out") expr)
        in (topdef, code, mainloop)
    | otherwise     = ("", "", "")
genDef (VarDef tsym expr) = ("", def, "")
    where   def = genTsym tsym ++ "(" ++ exprToLambda expr ++ ");\n"
genDef (Struct sym tsyms) = ("", "", "")


exprToLambda :: Expr -> [Char]
exprToLambda expr = "[]() { return " ++ genExpr expr ++  "; }"


genExpr :: Expr -> [Char]
genExpr expr = "fakeexpr"


genTsym :: Tsym -> [Char]
genTsym (Tsym t (Symbol s)) = genType t ++ " " ++ s


genRawType :: Type -> [Char]
genRawType (Type (Symbol s)) = s
genRawType (ListType t) = "std::vector<" ++ genType t ++ ">"
genRawType (TupleType ts) = "std::tuple<" ++ types ++ ">"
    where types = intercalate ", " $ map genType ts
genRawType ft = genType ft
