module Generators where

import Data.List
import Data.Ord

import AST

genDefs :: [Def] -> ([Char], [Char], [Char])
genDefs [] = ("you", "done", "goofed")
genDefs [def] = genDef def
genDefs (def:defs) = (topdef ++ topdefs, code ++ codes, mainloop)
    where   (topdef, code, mlNew) = genDef def
            (topdefs, codes, mlOld) = genDefs defs
            mainloop = maximumBy (comparing length) [mlNew, mlOld]


genDef :: Def -> ([Char], [Char], [Char])
genDef (FuncDef (Symbol sym) tsyms t expr)
    | sym == "main" = 
        let mainloop = "bool b=true;\nwhile(" ++ conds ++ ") B = !B;\n"
            conds = intercalate "&&" $ map cond [1..numSigs]
            cond n = "out.get(" ++ show (n-1) ++ ").fillBuffer(B)" 
            numSigs = length ts
            TupleType ts = t

            (topdef, code, _) = genDef (VarDef (Tsym t $ Symbol "out") expr)
        in (topdef, code, mainloop)
    | otherwise     = ("", "", "")

genDef (VarDef tsym expr) = ("", def, "")
    where def = genTsym tsym ++ " = " ++ genExpr expr ++ ";\n"

-- TODO: This!
genDef (Struct sym tsyms) = ("", "", "")


-- TODO: This!
genExpr :: Expr -> [Char]
genExpr _ = "fakeexpr"


-- TODO: This!
genTsym :: Tsym -> [Char]
genTsym (Tsym t (Symbol s)) = genType t ++ " " ++ s


-- TODO: This!
genType :: Type -> [Char]
genType (Type (Symbol s)) = s
genType (ListType t) = "faketype"
genType (TupleType ts) = "std::tuple<" ++ types ++ ">"
    where types = intercalate "," $ map genType ts
genType (FuncType ts r) = "faketype"
