import qualified HappyParser
import qualified AlexToken

import System.Environment   
import System.Process
import System.Exit

import AST
import Generators
import SemanticAnalysis
import TypeCheck

import Control.Monad.Writer
import Data.List

main :: IO ()
main = getArgs >>= handleArgs

handleArgs :: [String] -> IO()
handleArgs (infile:outfile:[]) = do
    input <- readFile infile
    let tokens = AlexToken.scanTokens input
        parse = HappyParser.parse tokens
        (newparse, errors) = runWriter $ semAnalysis parse
        code = if null errors then generateCode newparse else ""
    if (null errors)
      then do
        indent <- return code -- readProcess "astyle" [] code
        writeFile outfile indent
      else do
        putStrLn $ "Error: " ++ intercalate "\n" (map show errors)
        exitWith (ExitFailure 1)
handleArgs _ = do
    execname <- getProgName
    putStrLn $ "Usage:\n\t" ++ execname ++ " input.psl output.cpp"

generateCode :: AST -> [Char]
generateCode ast = header ++ n:sdecs ++ n:sdefs ++
                    n:topdecs ++ n:maindef ++ code ++ n:mainloop ++ footer ++ "}"
    where   header = "#include <iostream>\n#include <functional>\n#include \"outputs.h\"\n\n"
                  ++ "psl::Chunk<char> chr2Chunk(char chr) { return [=]{return chr;}; }\n\n"
            (sdecs, sdefs, topdecs, code, mainloop) = genTopDefs ast
            -- TODO: make maindef parse arguments into strings
            maindef = "int main(int argc, char **argv) {\n"
            n = '\n'
            footer = "return 0;\n"
