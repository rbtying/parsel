module Generators2 where

import {-# SOURCE #-} Generators()

import AST

genExpr :: Expr -> [Char]
genExpr (Literal val unit) = show val
genExpr (Attr expr (Symbol sym)) = genExpr expr ++ "." ++ sym
