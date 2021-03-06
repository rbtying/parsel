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
genDef (FuncDef (Symbol sym) tsyms rt expr)
    | sym == "main" = 
        let mainloop = "psl::Chunk<std::vector<psl::Chunk<char>>> args[argc];\n"
                ++ "for (int i = 0; i < argc; i++) {\n"
                ++ "std::vector<psl::Chunk<char>> chk(strlen(argv[i])+1);\n"
                ++ "std::transform(argv[i], "
                ++ "argv[i]+strlen(argv[i])+1, chk.begin(), chr2Chunk);\n"
                ++ "psl::set(args[i], psl::toChunk([=]{ return chk; }));\n"
                ++ "}\n\n"
                ++ "bool B = false, success;\n"
                ++ "auto fc = out()(" ++ args ++ ");\n"
                ++ concat (map dec [tsize,tsize-1..1])
                ++ "do {\n"
                ++ "B = !B;\n"
                ++ "success = " ++ fills ++ ";\n"
                ++ "} while (success);\n"
            dec n =
                let name = "psl::Chunk<psl::Signal> writer"
                    arg1 = "args[argc+(" ++ show (n - 2 - tsize) ++ ")], "
                    arg2 = "fc[" ++ show (n - 1) ++ "], "
                    arg3 = "args[argc-1]"
                    call = "(psl::makeWriter(" ++ arg1 ++ arg2 ++ arg3 ++ "))"
                in name ++ show n ++ call ++ ";\n"
            fills = intercalate " | " $ map fill [1..tsize]
            fill n = "writer" ++ show n ++ "().fillBuffer(B)"
            args = intercalate ", " . map arg $ reverse [1..numSigs]
            arg n = "args[" ++ show n ++ "]" 
            numSigs = length tsyms
            TupleType ts = rt
            rt' = ListType . Type . Symbol $ "signal"
            tsize = length ts

            (topdef, code, _) = genDef (FuncDef (Symbol "out") tsyms rt' expr)
        in (topdef, code, mainloop)
    | otherwise = 
        let def = "psl::set(" ++ sym ++ ", " ++
                    genExpr (Lambda tsyms rt expr) ++ ");\n"
            decl = (genTsym $ Tsym (FuncType argtypes rt) (Symbol sym)) ++ ";\n"
            argtypes = map (\ts -> let Tsym t _ = ts in t) tsyms
        in (decl, def, "")

genDef (VarDef tsym expr) = (decl, def, "")
    where   def = "psl::set(" ++ sym ++ ", " ++ genExpr expr ++  ");\n"
            Tsym _ (Symbol sym) = tsym
            decl = genTsym tsym ++ ";\n"


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
