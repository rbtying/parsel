{
module HappyParser where

import AlexToken
import AST
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    struct                        { TokenStruct _ }
    let                           { TokenLet _ }
    with                          { TokenWith _ }
    in                            { TokenIn _ }
    if                            { TokenIf _ }
    else                          { TokenElse _ }
    then                          { TokenThen _ }
    '->'                          { TokenArrow _ }
    and                           { TokenAnd _ }
    or                            { TokenOr _ }
    '<='                          { TokenLessThanEq _ }
    '>='                          { TokenGreaterThanEq _ }
    '!='                          { TokenNotEq _ }
    '=='                          { TokenEqEq _ }
    ','                           { TokenComma _ }
    '.'                           { TokenDot _ }
    '='                           { TokenEq _ }
    '\\'                          { TokenLambda _ }
    '+'                           { TokenAdd _ }
    '-'                           { TokenSub _ }
    '*'                           { TokenMul _ }
    '/'                           { TokenDiv _ }
    '<'                           { TokenLessThan _ }
    '>'                           { TokenGreaterThan _ }
    '!'                           { TokenNot _ }
    '['                           { TokenLBracket _ }
    ']'                           { TokenRBracket _ }
    '('                           { TokenLParen _ }
    ')'                           { TokenRParen _ }
    num                           { TokenNum _ _ }
    sym                           { TokenSym _ _ }
    string                        { TokenString _ _ }

%right '='
%left if else then
%left let in
%right '\\'
%left expr
%left with
%left ','
%left '('
%left and or
%left '<' '>' '<=' '>=' '!=' '=='
%left '+' '-'
%left '*' '/'
%right '!'
%left '.'
%%

AST : TopDefs                                       { $1 }

TopDefs : TopDef                                    { [$1] }
        | TopDefs TopDef                            { $2 : $1 }

TopDef : Def                                        { Def $1 }
       | struct Symbol '(' Tsyms ')'                { Struct $2 $4 }

Defs : Def                                          { [$1] }
     | Defs Def                                     { $2 : $1 }

Def : Symbol '(' Tsyms ')' '->' Type '=' Expr       { FuncDef $1 $3 $6 $8 }
    | Tsym '=' Expr                                 { VarDef $1 $3 }

Exprs : Expr %prec expr                             { [$1] }
      | Exprs ',' Expr                              { $3 : $1 }

Expr : Literal                                      { $1 }
     | Expr '.' Symbol                              { (Attr $1 $3, getInd $2) }
     | '(' Exprs ')'                                { (Tuple $2, getInd $1) }
     | '[' Exprs ']'                                { (List $2, getInd $1) }
     | sym                                          { (Var . Symbol $ getString $1, getInd $1) }
     | ApplyFunc                                    { $1 }
     | Lambda                                       { $1 }
     | LetExp                                       { $1 }
     | if Expr then Expr else Expr                  { (Cond $2 $4 $6, getInd $1) }
     | Op                                           { $1 }
     | string                                       { (Str $ getString $1, getInd $1) }

LetExp : let Defs in Expr                           { (LetExp $2 $4 , getInd $1) }

ApplyFunc : Expr '(' Exprs ')'                      { (Func $1 $3, getInd $2) }
          | Expr with Exprs                         { (Func $1 $3, getInd $2) }
          | Expr '\\' Symbol '\\' Expr              { (Func (Var $3, getInd $2) [$1, $5], getInd $2) }

Lambda : '\\' '(' Tsyms ')' '->' Type '=' Expr      { (Lambda $3 $6 $8, getInd $1) }

Tsyms : Tsym                                        { [$1] }
      | Tsyms ',' Tsym                              { $3 : $1 }

Tsym : Symbol Symbol                                { Tsym (Type $1) $2 }
     | Symbol '(' Types ')' '->' Type               { Tsym (FuncType $3 $6) $1 }
     | Symbol '(' Types ')'                         { Tsym (TupleType $3) $1 }
     | Symbol '[' Type ']'                          { Tsym (ListType $3) $1 }

Types : Type                                        { [$1] }
      | Types ',' Type                              { $3 : $1 }

Type : Symbol                                       { Type $1 }
     | '[' Type ']'                                 { ListType $2 }
     | '(' Types ')'                                { TupleType $2 }
     | '(' Types ')' '->' Type                      { FuncType $2 $5 }

Symbol : sym                                        { Symbol $ getString $1 }

Op   : Expr '+' Expr                                { (BinaryOp Plus $1 $3, getInd $2) }
     | Expr '-' Expr                                { (BinaryOp Minus $1 $3, getInd $2) }
     | Expr '/' Expr                                { (BinaryOp Divide $1 $3, getInd $2) }
     | Expr '*' Expr                                { (BinaryOp Multiply $1 $3, getInd $2) }
     | Expr '==' Expr                               { (BinaryOp Eq $1 $3, getInd $2) }
     | Expr '<' Expr                                { (BinaryOp LessThan $1 $3, getInd $2) }
     | Expr '>' Expr                                { (BinaryOp GreaterThan $1 $3, getInd $2) }
     | Expr '<=' Expr                               { (BinaryOp LessThanEq $1 $3, getInd $2) }
     | Expr '>=' Expr                               { (BinaryOp GreaterThanEq $1 $3, getInd $2) }
     | Expr and Expr                                { (BinaryOp And $1 $3, getInd $2) }
     | Expr or Expr                                 { (BinaryOp Or $1 $3, getInd $2) }
     | Expr '!=' Expr                               { let e = (BinaryOp Eq $1 $3, getInd $2)
                                                      in (UnaryOp Negate e, getInd $2) }
     | '!' Expr                                     { (UnaryOp Negate $2, getInd $1) }
     | '-' Expr %prec '!'                           { (UnaryOp Negate $2, getInd $1) }

Literal : num                                       { (toLiteral $ getNum $1, getInd $1) }

{
parseError :: [Token] -> a
parseError x = error $ "Parse error: " ++ (show x)

toLiteral :: (Float, [Char]) -> Expr
toLiteral (n, u) = Literal n u
}
