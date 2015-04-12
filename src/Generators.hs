module Generators where

import Data.List
import Data.Ord

import AST
import Generators2

-- sdecs, sdefs, topdecs, code, mainloop
genTopDefs :: [TopDef] -> ([Char], [Char], [Char], [Char], [Char])
genTopDefs [] = ("", "", "", "", "")
genTopDefs [td] = genTopDef td
genTopDefs (td:tds) = (sdec ++ sdecs, sdef ++ sdefs, d ++ ds, c ++ cs, ml)
    where   (sdec, sdef, d, c, mlNew) = genTopDef td
            (sdecs, sdefs, ds, cs, mlOld) = genTopDefs tds
            ml = maximumBy (comparing length) [mlNew, mlOld]


-- sdecs, sdefs, topdecs, code, mainloop
genTopDef :: TopDef -> ([Char], [Char], [Char], [Char], [Char])
genTopDef (Def d) = ("", "", td, c, ml)
    where (td, c, ml) = genDef d
genTopDef (Struct (Symbol sym) tsyms) = (sdec, sdef, "", "", "")
    where   sdec = "struct " ++ sym ++ ";\n"
            sdef = "struct " ++ sym ++ " {\n" ++ members ++ "\n};\n"
            members = intercalate ";\n" $ map genTsym tsyms


-- topdecs, code
genDefs :: [Def] -> ([Char], [Char])
genDefs [] = ("", "")
genDefs [def] = (topdef, code)
    where (topdef, code, _) = genDef def
genDefs (def:defs) = (topdef ++ topdefs, code ++ codes)
    where   (topdef, code, _) = genDef def
            (topdefs, codes) = genDefs defs


-- topdecs, code, mainloop
genDef :: Def -> ([Char], [Char], [Char])
genDef (FuncDef (Symbol sym) tsyms rt expr)
    | sym == "main" = 
        let mainloop = "bool B = true;\nwhile(" ++ conds ++ ") B = !B;\n"
            conds = intercalate " && " $ map cond [1..numSigs]
            cond n = "out().get(" ++ show (n-1) ++ ")().fillBuffer(B)" 
            numSigs = length ts
            TupleType ts = rt

            (topdef, code, _) = genDef (VarDef (Tsym rt $ Symbol "out") expr)
        in (topdef, code, mainloop)
    | otherwise = 
        let def = sym ++ " = " ++ lambda
            lambda = "[&](" ++ args ++ ") { return " ++ genExpr expr ++ "; };\n"
            args = intercalate ", " $ map genRawTsym tsyms
            
            decl = (genTsym $ Tsym (FuncType argtypes rt) (Symbol sym)) ++ ";\n"
            argtypes = map (\ts -> let Tsym t _ = ts in t) tsyms
        in (decl, def, "")

genDef (VarDef tsym expr) = (decl, def, "")
    where   def = sym ++ " = [&]() { return " ++ genExpr expr ++  "; };\n"
            Tsym _ (Symbol sym) = tsym
            decl = genTsym tsym ++ ";\n"


genType :: Type -> [Char]
genType (FuncType ts r) = "psl::Chunk<" ++ types ++ ">"
    where types = intercalate ", " $ map genRawType (r:ts)
genType t = wrapInChunk . genRawType $ t
    where   wrapInChunk s = "psl::Chunk<" ++ s ++ ">"


genTsym :: Tsym -> [Char]
genTsym (Tsym t (Symbol s)) = genType t ++ " " ++ s


genRawTsym :: Tsym -> [Char]
genRawTsym (Tsym t (Symbol s)) = genRawType t ++ " " ++ s


genRawType :: Type -> [Char]
genRawType (Type (Symbol s))
    | s == "signal"     = "psl::Signal"
    | s == "complex"    = "std::complex<double>"
    | otherwise         = s
genRawType (ListType t) = "std::vector<" ++ genType t ++ ">"
genRawType (TupleType ts) = "std::tuple<" ++ types ++ ">"
    where types = intercalate ", " $ map genType ts
genRawType ft = genType ft
