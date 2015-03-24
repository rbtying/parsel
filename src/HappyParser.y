{
module HappyParser where

import AlexToken
import AST
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    let                           { TokenLet _ }
    with                          { TokenWith _ }
    in                            { TokenIn _ }
    if                            { TokenIf _ }
    else                          { TokenElse _ }
    then                          { TokenThen _ }
    unless                        { TokenUnless _ }
    '->'                          { TokenArrow _ }
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
    num                           { TokenNum _ $$ }
    unit                          { TokenUnit _ $$ }
    sym                           { TokenSym _ $$ }

%left '+' '-'
%left '*' '/'
%%

AST : Defs                                          { $1 }

Defs : Def                                          { [$1] }
     | Defs Def                                     { $2 : $1 }

Def : Symbol '(' Tsyms ')' '->' Type '=' Expr       { FuncDef $1 $3 $6 $8 }
    | Symbol '=' Expr                               { VarDef $1 $3 }

Exprs : Expr                                        { [$1] }
      | Exprs Expr                                  { $2 : $1 }

Expr : Literal                                      { $1 }
     | Expr '.' Symbol                              { Attr $1 $3 }
     | '(' Args ')'                                 { Tuple $2 }
     | '[' Args ']'                                 { List $2 }
     | Op                                           { $1 }
     | Symbol                                       { Var $1 }
     | ApplyFunc                                    { $1 }
     | Lambda                                       { $1 }
     | LetExp                                       { $1 }
     | if Expr then Expr else Expr                  { Cond $2 $4 $6 }

LetExp : let Defs in Expr                           { LetExp $2 $4 }

ApplyFunc : Symbol '(' Args ')'                     { Func $1 $3 }
          | Symbol with Args                        { Func $1 $3 }

Args : Expr                                         { [$1] }
     | Args ',' Expr                                { $3 : $1 }

Lambda : '\\' '(' Tsyms ')' '->' Type '=' Expr      { Lambda $3 $6 $8 }

Tsyms : Tsym                                        { [$1] }
      | Tsyms ',' Tsym                              { $3 : $1 }

Tsym : Type Symbol                                  { Tsym $1 $2 }
     | Symbol FType                                 { Tsym $2 $1 }

Types : Type                                        { [$1] }
      | Types ',' Type                              { $3 : $1 }

Type : Symbol                                       { Type $1 }
     | '[' Symbol ']'                               { ListType $2 }
     | FType                                        { $1 }

FType : '(' Types ')' '->' Type                     { FuncType $2 $5 }

Symbol : sym                                        { Symbol $1 }

Op   : Expr '+' Expr                                { BinaryOp Plus $1 $3 }
     | Expr '-' Expr                                { BinaryOp Minus $1 $3 }
     | Expr '/' Expr                                { BinaryOp Divide $1 $3 }
     | Expr '*' Expr                                { BinaryOp Multiply $1 $3 }
     | Expr '=' '=' Expr                            { BinaryOp Eq $1 $4 }
     | Expr '<' Expr                                { BinaryOp LessThan $1 $3 }
     | Expr '>' Expr                                { BinaryOp GreaterThan $1 $3 }
     | Expr '<' '=' Expr                            { BinaryOp LessThanEq $1 $4 }
     | Expr '>' '=' Expr                            { BinaryOp GreaterThanEq $1 $4 }
     | Expr '!' '=' Expr                            { UnaryOp Negate (BinaryOp Eq $1 $4) }
     | '!' Expr                                     { UnaryOp Negate $2 }

Literal : num unit                                  { Literal $1 $2 }
        | num                                       { Literal $1 "" }
        | '-' num unit                              { UnaryOp Negate (Literal $2 $3) }
        | '-' num                                   { UnaryOp Negate (Literal $2 "") }

{
parseError :: [Token] -> a
parseError x = error $ "Parse error: " ++ (show x)
}
