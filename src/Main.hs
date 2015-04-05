import qualified HappyParser
import qualified AlexToken

import System.Environment   

import AST
import Generators

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
