module GlobalScope where

import AST

import qualified Data.Map as Map

type SymbolTable = Map.Map Symbol [Type]
type StructData = Map.Map Type (Map.Map Symbol Type)

--TODO: finish this!
builtInScope :: SymbolTable
builtInScope = Map.fromList table
    where   table = [ (Symbol "sin", [FuncType [f] c])
                    , (Symbol "cos", [FuncType [f] f])
                    , (Symbol "ft", [FuncType [s] fs])
                    , (Symbol "ift", [FuncType [fs] s])
                    , (Symbol "length", [FuncType [str] f]) 
                    , (Symbol "intervalMap", [FuncType [t, im, s] s])
                    , (Symbol "signal", [FuncType [fm] s])
                    , (Symbol "loadSignal", [FuncType [str] s])
                    , (Symbol "toComplex", [FuncType [f] c])
                    , (Symbol "fromComplex", [FuncType [c] f])
                    , (Symbol "psl::greaterThan", compType)
                    , (Symbol "psl::multiply", arithmeticType)
                    ]
            f   = Type . Symbol $ "float"
            z   = Type . Symbol $ "int"
            h   = Type . Symbol $ "freq"
            s   = Type . Symbol $ "signal"
            fs  = Type . Symbol $ "fsignal"
            t   = Type . Symbol $ "time"
            i   = Type . Symbol $ "interval"
            b   = Type . Symbol $ "bool"
            c   = Type . Symbol $ "complex"
            str = ListType . Type . Symbol $ "char"
            im  = FuncType [i] i
            fm  = FuncType [f] c
            arithmeticType =    [ FuncType [f, f] f
                                , FuncType [z, z] z
                                , FuncType [c, c] c
                                , FuncType [t, t] t
                                , FuncType [h, h] h ]
            compType =  [ FuncType [f, f] b
                        , FuncType [z, z] b
                        , FuncType [t, t] b
                        , FuncType [h, h] b ]

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
