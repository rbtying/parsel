module Generators2 where

import {-# SOURCE #-} Generators (genRawType)

import AST
import Data.List

genType :: Type -> [Char]
genType (FuncType ts r) = "std::function<" ++ genRawType r ++ "(" ++ args ++ ")>"
    where args = intercalate ", " $ map genRawType ts
genType t = wrapInFunc . genRawType $ t
    where   wrapInFunc s = "std::function<" ++ s ++ "()>"
