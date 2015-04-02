import qualified HappyParser
import qualified AlexToken
import AST

import Data.List
import Data.Ord
import System.Environment   


main :: IO ()
main = do
    (a1:(a2:_)) <- getArgs
    input <- readFile a1
    let tokens = AlexToken.scanTokens input
        parse = HappyParser.parse tokens
        semantics = semAnalysis parse
        code =  if semantics == Good
                then generateCode parse
                else "Error: " ++ show semantics
    writeFile a2 code


data Semantics  = Good
                | TypeError Type Type
                | Undef Symbol
                | NoMain
                deriving (Show, Eq)


semAnalysis :: AST -> Semantics
semAnalysis _ = Good


generateCode :: AST -> [Char]
generateCode ast = header ++ topdefs ++ maindef ++ code ++ mainloop ++ "}"
    where   header = "#include <iostream>\n#include \"outputs.h\"\n"
            (topdefs, code, mainloop) = genDefs ast
            -- TODO: make maindef parse arguments into strings
            maindef = "int main(int argc, char **argv) {\n"


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
