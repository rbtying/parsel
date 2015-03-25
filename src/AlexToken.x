{
{-# OPTIONS_GHC -w #-}
module AlexToken (Token(..),scanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$eol   = [\n]

tokens :-

  $eol                          ;
  $white+                       ;
  "#".*                         ;
  let                           { tok (\p s -> TokenLet p) }
  with                          { tok (\p s -> TokenWith p) }
  in                            { tok (\p s -> TokenIn p) }
  if                            { tok (\p s -> TokenIf p) }
  else                          { tok (\p s -> TokenElse p) }
  then                          { tok (\p s -> TokenThen p) }
  "->"                          { tok (\p s -> TokenArrow p) }
  and                           { tok (\p s -> TokenAnd p) }
  or                            { tok (\p s -> TokenOr p) }
  "<="                          { tok (\p s -> TokenLessThanEq p) }
  ">="                          { tok (\p s -> TokenGreaterThanEq p) }
  "!="                          { tok (\p s -> TokenNotEq p) }
  "=="                          { tok (\p s -> TokenEqEq p) }
  \=                            { tok (\p s -> TokenEq p) }
  \\                            { tok (\p s -> TokenLambda p) }
  \!                            { tok (\p s -> TokenNot p) }
  \+                            { tok (\p s -> TokenAdd p) }
  \-                            { tok (\p s -> TokenSub p) }
  \*                            { tok (\p s -> TokenMul p) }
  \/                            { tok (\p s -> TokenDiv p) }
  \<                            { tok (\p s -> TokenLessThan p) }
  \>                            { tok (\p s -> TokenGreaterThan p) }
  \[                            { tok (\p s -> TokenLBracket p) }
  \]                            { tok (\p s -> TokenRBracket p) }
  \(                            { tok (\p s -> TokenLParen p) }
  \)                            { tok (\p s -> TokenRParen p) }
  [KMmu]?(Hz|s|min|hour)        { tok (\p s -> TokenUnit p s) }
  $digit+(\.$digit+)?           { tok (\p s -> TokenNum p (read s :: Float)) }
  $alpha [$alpha $digit \_ \']* { tok (\p s -> TokenSym p s) }
  [\,]                          { tok (\p s -> TokenComma p) }
  [\.]                          { tok (\p s -> TokenDot p) }

{

tok f p s = f p s

data Token = TokenLet AlexPosn
           | TokenWith AlexPosn
           | TokenIn AlexPosn
           | TokenIf AlexPosn
           | TokenElse AlexPosn
           | TokenThen AlexPosn
           | TokenUnless AlexPosn
           | TokenLambda AlexPosn
           | TokenArrow AlexPosn
           | TokenAnd AlexPosn
           | TokenOr AlexPosn
           | TokenLessThanEq AlexPosn
           | TokenGreaterThanEq AlexPosn
           | TokenNotEq AlexPosn
           | TokenEqEq AlexPosn
           | TokenEq AlexPosn
           | TokenAdd AlexPosn
           | TokenSub AlexPosn
           | TokenMul AlexPosn
           | TokenDiv AlexPosn
           | TokenComma AlexPosn
           | TokenDot AlexPosn
           | TokenLessThan AlexPosn
           | TokenGreaterThan AlexPosn
           | TokenLParen AlexPosn
           | TokenRParen AlexPosn
           | TokenLBracket AlexPosn
           | TokenRBracket AlexPosn
           | TokenUnit AlexPosn String
           | TokenNum AlexPosn Float
           | TokenSym AlexPosn String
           | TokenNot AlexPosn
           deriving (Eq,Show)

scanTokens = alexScanTokens

token_posn (TokenLet p) = p
token_posn (TokenWith p) = p
token_posn (TokenIn p) =  p
token_posn (TokenIf p) = p
token_posn (TokenElse p) = p
token_posn (TokenThen p) = p
token_posn (TokenUnless p) = p
token_posn (TokenLambda p) = p
token_posn (TokenArrow p) = p
token_posn (TokenAnd p) = p
token_posn (TokenOr p) = p
token_posn (TokenLessThanEq p) = p
token_posn (TokenGreaterThanEq p) = p
token_posn (TokenNotEq p) = p
token_posn (TokenEqEq p) = p
token_posn (TokenEq p) = p
token_posn (TokenAdd p) = p
token_posn (TokenSub p) = p
token_posn (TokenMul p) = p
token_posn (TokenDiv p) = p
token_posn (TokenComma p) = p
token_posn (TokenDot p) = p
token_posn (TokenLessThan p) = p
token_posn (TokenGreaterThan p) = p
token_posn (TokenLParen p) = p
token_posn (TokenRParen p) = p
token_posn (TokenLBracket p) = p
token_posn (TokenRBracket p) = p
token_posn (TokenUnit p _) = p
token_posn (TokenSym p _) = p
token_posn (TokenNum p _) = p
token_posn (TokenNot p) = p

}