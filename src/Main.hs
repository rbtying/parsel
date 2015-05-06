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

handleArgs :: [String] -> IO ()
handleArgs (infile:[]) = do
    input <- readFile infile
    let tokens = AlexToken.scanTokens input
        parse = HappyParser.parse tokens
        (newparse, errors) = runWriter $ semAnalysis parse
    if True --(null errors)
        then do
            writeFile "main.cpp" $ generateCode parse --newparse
            process <- runCommand "make"
            _ <- waitForProcess process
            return ()
        else do
            putStrLn $ "Error: " ++ intercalate "\n" (map show errors)
            exitWith (ExitFailure 1)
handleArgs _ = do
    execname <- getProgName
    putStrLn $ "Usage:\n\t" ++ execname ++ " input.psl"

generateCode :: AST -> [Char]
generateCode ast = header ++ n:sdecs ++ n:sdefs ++ n:topdecs ++ n:maindef ++
                   code ++ n:mainloop ++ footer ++ "}"
    where   header = "#include <iostream>\n#include <functional>\n#include "
                     ++ "\"includes.h\"\n\npsl::Chunk<char> chr2Chunk(char chr)"
                     ++ " { psl::Chunk<char> c([=]{return chr;}); return c; }\n\n"
            (sdecs, sdefs, topdecs, code, mainloop) = genTopDefs ast
            maindef = "int main(int argc, char **argv) {\n"
            n = '\n'
            footer = "return 0;\n"
