import qualified HappyParser
import qualified AlexToken

import System.Environment   
import System.Process

import AST
import Generators

main :: IO ()
main = do
    (infile:(outfile:_)) <- getArgs
    input <- readFile infile
    let tokens = AlexToken.scanTokens input
        parse = HappyParser.parse tokens
        (semantics, newparse) = semAnalysis parse
        code =  if semantics == Good
                then generateCode newparse
                else "Error: " ++ show semantics
    indent <- readProcess "astyle" [] code
    writeFile outfile indent


data Semantics  = Good
                | TypeError Type Type
                | Undef Symbol
                | NoMain
                deriving (Show, Eq)


semAnalysis :: AST -> (Semantics, AST)
semAnalysis ast = (Good, ast)


generateCode :: AST -> [Char]
generateCode ast = header ++ n:sdecs ++ n:sdefs ++
                    n:topdecs ++ n:maindef ++ code ++ n:mainloop ++ footer ++ "}"
    where   header = "#include <iostream>\n#include \"outputs.h\"\n"
            (sdecs, sdefs, topdecs, code, mainloop) = genTopDefs ast
            -- TODO: make maindef parse arguments into strings
            maindef = "int main(int argc, char **argv) {\n"
            n = '\n'
            footer = "return 0;\n"
