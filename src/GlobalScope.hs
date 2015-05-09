module GlobalScope where

import AST

import qualified Data.Map as Map
import TypeCheck

--TODO: finish this!
builtInScope :: SymbolTable
builtInScope = Map.fromList table
    where   table = [ (Symbol "sin", FuncType [f] f)
                    , (Symbol "cos", FuncType [f] f)
                    , (Symbol "ft", FuncType [s] fs)
                    ]
            f = Type . Symbol $ "float"
            s = Type . Symbol $ "signal"
            fs = Type . Symbol $ "fsignal"

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
