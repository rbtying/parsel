import qualified HappyParser
import qualified AlexToken

import System.Environment   
import System.Process

import AST
import Generators
import SemanticAnalysis
import TypeCheck

import Control.Monad.Writer

main :: IO ()
main = do
    (infile:(outfile:_)) <- getArgs
    input <- readFile infile
    let tokens = AlexToken.scanTokens input
        parse = HappyParser.parse tokens
        (newparse, semantics) = runWriter $ semAnalysis parse
        errors = filter (/= Good) semantics 
        code =  if null errors
                then generateCode newparse
                else "Error: " ++ show errors
    indent <- return code --readProcess "astyle" [] code
    writeFile outfile indent



generateCode :: AST -> [Char]
generateCode ast = header ++ n:sdecs ++ n:sdefs ++
                    n:topdecs ++ n:maindef ++ code ++ n:mainloop ++ footer ++ "}"
    where   header = "#include <iostream>\n#include \"outputs.h\"\n"
            (sdecs, sdefs, topdecs, code, mainloop) = genTopDefs ast
            -- TODO: make maindef parse arguments into strings
            maindef = "int main(int argc, char **argv) {\n"
            n = '\n'
            footer = "return 0;\n"
