module Generators where

import AST

genRawType :: Type -> [Char]
genType :: Type -> [Char]
genDefs :: [Def] -> ([Char], [Char])
genTsym :: Tsym -> [Char]
