module GlobalScope where

import AST

import qualified Data.Map as Map
import TypeCheck

builtInScope :: VarScope
builtInScope = Map.fromList [(0, Node builtInSymbolTable [] Empty)]
    where   builtInSymbolTable = Map.fromList [] :: SymbolTable

builtInStructData :: StructData
builtInStructData = Map.fromList [
            builtInType "interval",
            builtInType "time",
            builtInType "signal",
            builtInType "fsignal"
    ]
    where builtInType n = (t, Map.fromList [(s, t)])
              where s = Symbol n :: Symbol
                    t = Type s :: Type
