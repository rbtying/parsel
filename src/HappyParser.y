{
module HappyParser where

import AlexToken
}

%name parse
%tokentype { Token }
%error { parseError }

%token
    let                           { TokenLet $$ }
    in                            { TokenIn $$ }
    if                            { TokenIf $$ }
    else                          { TokenElse $$ }
    then                          { TokenThen $$ }
    '->'                          { TokenArrow $$ }
    ','                           { TokenComma $$ }
    '.'                           { TokenDot $$ }
    '='                           { TokenEq $$ }
    '\\'                          { TokenLambda $$ }
    '+'                           { TokenAdd $$ }
    '-'                           { TokenSub $$ }
    '*'                           { TokenMul $$ }
    '/'                           { TokenDiv $$ }
    '<'                           { TokenLessThan $$ }
    '>'                           { TokenGreaterThan $$ }
    "<="                          { TokenLessThanEq $$ }
    ">="                          { TokenGreaterThanEq $$ }
    '['                           { TokenLBracket $$ }
    ']'                           { TokenRBracket $$ }
    '('                           { TokenLParen $$ }
    ')'                           { TokenRParen $$ }
    num                           { TokenNum $$ _ }
    unit                          { TokenUnit $$ _ }
    prefix                        { TokenPrefix $$ _ }
    sym                           { TokenSym $$ _ }

%left '+' '-'
%left '*' '/'
%%

Var : sym                               { }
    | sym '.' sym                       { }

Num : num                               { }
    | num Unit                          { }

Unit : unit                             { }
     | prefix unit                      { }

Expr : let Var '=' Expr in Expr         { }
     | '\\' Args '->' Expr              { }
     | Form                             { }

Form : Form '+' Form                    { }
     | Form '-' Form                    { }
     | Form '/' Form                    { }
     | Form '*' Form                    { }
     | Atom                             { }

Args : '(' Args ')'                     { }
     | Var ',' Var                      { }
     | Var                              { }

Atom : '(' Expr ')'                     { }
     | Var                              { }
     | Num                              { }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"
}
