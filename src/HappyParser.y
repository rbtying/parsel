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

AST : Exprs                                         { Tuple $1 }

Exprs : Expr                                        { [$1] }
      | Exprs Expr                                  { $2 : $1 }

Expr : Literal                                      { $1 }
     | Expr '.' Symbol                              { Attr $1 $3 }
     | '(' Args ')'                                 { Tuple $2 }
     | '[' Args ']'                                 { List $2 }
     | Op                                           { $1 }
     | Symbol                                       { Var $1 }
     | Func                                         { $1 }
     | ApplyFunc                                    { $1 }
     | LetExp                                       { $1 }
     | WithExp                                      { $1 }
     | if Expr then Expr else Expr                  { Cond $2 $4 $6 }
     | if Expr then Expr                            { Cond $2 $4 Nop }
     | unless Expr then Expr                        { Cond $2 Nop $4 }

LetExp : let Symbol '=' Expr in Expr                { Let $2 $4 $6 }

WithExp : Symbol with Args                          { Apply $1 $3 }

ApplyFunc : Symbol '(' Args ')'                     { Apply $1 $3 }

Args : Expr                                         { [$1] }
     | Args ',' Expr                                { $3 : $1 }

Func : Symbol '(' Tsyms ')' '->' Type '=' Expr      { Func $1 $3 $6 $8 }
     | '\\' '(' Tsyms ')' '->' Type '=' Expr        { Lambda $3 $6 $8 }

Tsyms : Tsym                                        { [$1] }
      | Tsyms ',' Tsym                              { $3 : $1 }

Tsym : Type Symbol                                  { TypedSymbol $1 $2 }
     | Symbol FType                                 { TypedSymbol $2 $1 }

Types : Type                                        { [$1] }
      | Types ',' Type                                  { $3 : $1 }

Type : Symbol                                       { Type $1 }
     | '[' Symbol ']'                               { ListType $2 }
     | FType                                        { $1 }

FType : '(' Types ')' '->' Type                     { FuncType $2 $5 }

Symbol : sym                                        { Symbol $1 }

Op   : Expr '+' Expr                                { BinaryOp AddOp $1 $3 }
     | Expr '-' Expr                                { BinaryOp SubOp $1 $3 }
     | Expr '/' Expr                                { BinaryOp DivOp $1 $3 }
     | Expr '*' Expr                                { BinaryOp MulOp $1 $3 }
     | Expr '=' '=' Expr                            { BinaryOp EqOp $1 $4 }
     | Expr '<' Expr                                { BinaryOp LessThanOp $1 $3 }
     | Expr '>' Expr                                { BinaryOp GreaterThanOp $1 $3 }
     | Expr '<' '=' Expr                            { BinaryOp LessThanEqOp $1 $4 }
     | Expr '>' '=' Expr                            { BinaryOp GreaterThanEqOp $1 $4 }
     | Expr '!' '=' Expr                            { UnaryOp NegateOp (BinaryOp EqOp $1 $4) }
     | '!' Expr                                     { UnaryOp NegateOp $2 }

Literal : num unit                                  { Literal $2 (Value $1) }
        | num                                       { Literal "" (Value $1) }
        | '-' num                                   { UnaryOp NegateOp (Literal "" (Value $2)) }
        | '-' num unit                              { UnaryOp NegateOp (Literal $3 (Value $2)) }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
