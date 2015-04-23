{
{-# OPTIONS_GHC -w #-}
module AlexToken (Token(..), scanTokens, getInd, getString, getNum) where

import Data.List
import Data.Char
import Control.Arrow
}

%wrapper "posn"

$digit  = 0-9
$alpha  = [a-zA-Z]
$eol    = [\n]
@num    = [0-9]+(\.[0-9]+)?([KMmu]?(Hz|s|min|hour))?

tokens :-

  "#".*                         ;
  $white+                       ;
  $eol+                         ;
  struct                        { \p s -> TokenStruct p }
  let                           { \p s -> TokenLet p }
  with                          { \p s -> TokenWith p }
  in                            { \p s -> TokenIn p }
  if                            { \p s -> TokenIf p }
  else                          { \p s -> TokenElse p }
  then                          { \p s -> TokenThen p }
  "->"                          { \p s -> TokenArrow p }
  and                           { \p s -> TokenAnd p }
  or                            { \p s -> TokenOr p }
  "<="                          { \p s -> TokenLessThanEq p }
  ">="                          { \p s -> TokenGreaterThanEq p }
  "!="                          { \p s -> TokenNotEq p }
  "=="                          { \p s -> TokenEqEq p }
  \=                            { \p s -> TokenEq p }
  \\                            { \p s -> TokenLambda p }
  \!                            { \p s -> TokenNot p }
  \+                            { \p s -> TokenAdd p }
  \-                            { \p s -> TokenSub p }
  \*                            { \p s -> TokenMul p }
  \/                            { \p s -> TokenDiv p }
  \<                            { \p s -> TokenLessThan p }
  \>                            { \p s -> TokenGreaterThan p }
  \[                            { \p s -> TokenLBracket p }
  \]                            { \p s -> TokenRBracket p }
  \(                            { \p s -> TokenLParen p }
  \)                            { \p s -> TokenRParen p }
  @num                          { \p s -> TokenNum p (toNum s) }
  $alpha [$alpha $digit \_ \']* { \p s -> TokenSym p s }
  [\,]                          { \p s -> TokenComma p }
  [\.]                          { \p s -> TokenDot p }
  \"[^\\\"]*\"                  { \p s -> TokenString p s }

{

data Token = TokenStruct AlexPosn
           | TokenLet AlexPosn
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
           | TokenNum AlexPosn (Float, String)
           | TokenSym AlexPosn String
           | TokenNot AlexPosn
           | TokenString AlexPosn String
           deriving (Eq,Show)

scanTokens = alexScanTokens

toNum :: [Char] -> (Float, [Char])
toNum = (read *** id) . partition isDigit

token_posn (TokenStruct p) = p
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
token_posn (TokenSym p _) = p
token_posn (TokenNum p _) = p
token_posn (TokenNot p) = p
token_posn (TokenString p _) = p

getInd :: Token -> Int
getInd t = i
    where (AlexPn i _ _) = token_posn t

getString :: Token -> String
getString (TokenString _ s) = s
getString (TokenSym _ s) = s
getString _ = ""

getNum :: Token -> (Float, String)
getNum (TokenNum _ n) = n
getNum _ = (0, "")

}
