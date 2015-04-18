module Generators where

import AST

genRawType :: Type -> [Char]
genDefs :: [Def] -> ([Char], [Char])
genLambda :: Tsyms -> Expr -> [Char]
