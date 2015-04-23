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
            sdef = "struct " ++ sym ++ " {\n" ++ members ++ "};\n"
            members = concat $ map ((++";\n") . genTsym) tsyms


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
genDef (FuncDef (Symbol sym) tsyms rt iexpr)
    | sym == "main" = 
        let mainloop = "bool B = true;\nwhile(" ++ conds ++ ")\nB = !B;\n"
            conds = intercalate " && " $ map cond [1..numSigs]
            cond n = "out().get(" ++ show (n-1) ++ ")().fillBuffer(B)" 
            numSigs = length ts
            TupleType ts = rt

            (topdef, code, _) = genDef (VarDef (Tsym rt $ Symbol "out") iexpr)
        in (topdef, code, mainloop)
    | otherwise = 
        let def = sym ++ " = " ++ genExpr (Lambda tsyms rt iexpr) ++ ";\n"
            
            decl = (genTsym $ Tsym (FuncType argtypes rt) (Symbol sym)) ++ ";\n"
            argtypes = map (\ts -> let Tsym t _ = ts in t) tsyms
        in (decl, def, "")

genDef (VarDef tsym iexpr) = (decl, def, "")
    where   def = sym ++ " = [&]() {\n" ++ genReturn expr ++  "\n};\n"
            Tsym _ (Symbol sym) = tsym
            decl = genTsym tsym ++ ";\n"
            expr = fst iexpr


genType :: Type -> [Char]
genType t = wrapInChunk . genRawType $ t
    where   wrapInChunk s = "psl::Chunk<" ++ s ++ ">"


genTsym :: Tsym -> [Char]
genTsym (Tsym t (Symbol s)) = genType t ++ " " ++ s


genRawType :: Type -> [Char]
genRawType (Type (Symbol s))
    | s == "signal"     = "psl::Signal"
    | s == "complex"    = "std::complex<double>"
    | s == "float"      = "double"
    | s == "time"       = "double"
    | s == "freq"       = "double"
    | otherwise         = s
genRawType (ListType t) = "std::vector<" ++ genType t ++ ">"
genRawType (TupleType ts) = "std::tuple<" ++ types ++ ">"
    where types = intercalate ", " $ map genType ts
genRawType (FuncType argts rt) = "std::function<" ++ r ++ "(" ++ args ++ ")>"
    where   r = genRawType rt
            args = intercalate "," $ map genType argts
