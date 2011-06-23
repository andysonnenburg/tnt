{
{-# LANGUAGE DeriveFunctor, StandaloneDeriving #-}
{-# OPTIONS_GHC
    -fno-warn-lazy-unlifted-bindings
    -fno-warn-missing-signatures
    -fno-warn-name-shadowing
    -fno-warn-unused-binds
    -fno-warn-unused-matches#-}
module Language.TNT.Lexer
       ( Alex
       , AlexPosn (..)
       , alexError
       , alexGetInput
       , alexMonadScan
       , alexMonadScan'
       , runAlex
       ) where

import Codec.Binary.UTF8.String (decode)

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import Data.Word

import Language.TNT.Token

import Prelude hiding (Ordering (..), and, or)
}

%wrapper "monadUserState-bytestring"

$digit = 0-9
$alpha = [a-zA-Z]

@name = $alpha [$alpha $digit \_]*

:-
  <0> \n|$white ;
  <0> "//".* ;
  <0> "import" { import' }
  <0> "as" { as' }
  <0> "var" { var }
  <0> "fun" { fun }
  <0> "if" { if' }
  <0> "else" { else' }
  <0> "for" { for }
  <0> "in" { in' }
  <0> "return" { return' }
  <0> @name { name }
  <0> \" { beginString `andBegin` string }
  <string> \\ { begin escapedChar }
  <escapedChar> \" { char `andBegin` string }
  <string> \" { endString `andBegin` 0}
  <string> . { char }
  <0> "=" { equal }
  <0> "==" { eq }
  <0> "!=" { ne }
  <0> "<" { lt }
  <0> "<=" { le }
  <0> ">" { gt }
  <0> ">=" { ge }
  <0> "&&" { and }
  <0> "||" { or }
  <0> "." { dot }
  <0> "," { comma }
  <0> "(" { openParen }
  <0> ")" { closeParen }
  <0> "{" { openBrace }
  <0> "}" { closeBrace }
  <0> "[" { openBracket }
  <0> "]" { closeBracket }
  <0> ";" { semi }

{

deriving instance Functor Alex
  
type AlexUserState = [Word8] -> [Word8]

name :: AlexAction (Alex Token)
name (p, _, s) x = return a 
  where
    a = Name . toString $ BL.take (fromIntegral x) s

beginString :: AlexAction (Alex Token)
beginString _ _ = alexSetUserState id >>
                          alexMonadScan

char :: AlexAction (Alex Token)
char (_, _, s) n = do
  f <- alexGetUserState
  alexSetUserState $ f . g
  alexMonadScan
  where
    g xs = BL.unpack (BL.take (fromIntegral n) s) ++ xs

endString :: AlexAction (Alex Token)
endString _ _ = do
  f <- alexGetUserState
  return . String . decode . f $ []

import' = token' Import
as' = token' As
var = token' Var
fun = token' Fun
if' = token' If
else' = token' Else
for = token' For
in' = token' In
return' = token' Return
equal = token' Equal
eq = token' EQ
ne = token' NE
lt = token' LT
le = token' LE
gt = token' GT
ge = token' GE
and = token' And
or = token' Or
dot = token' Dot
comma = token' Comma
openParen = token' OpenParen
closeParen = token' CloseParen
openBrace = token' OpenBrace
closeBrace = token' CloseBrace
openBracket = token' OpenBracket
closeBracket = token' CloseBracket
semi = token' Semi

token' x _ _ = return x

alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (AlexPn _ l _, _, _) -> alexError $ show l ++ ": lexical error"
    AlexSkip inp' len -> do
      alexSetInput inp'
      alexMonadScan'
    AlexToken inp' len action -> do
      alexSetInput inp'
      action inp len

alexEOF :: Alex Token
alexEOF = return EOF

alexInitUserState :: AlexUserState
alexInitUserState = id

alexGetUserState :: Alex AlexUserState
alexGetUserState = Alex $ \s@AlexState { alex_ust = ust} -> Right (s, ust)

alexSetUserState :: AlexUserState -> Alex ()
alexSetUserState ust = Alex $ \s -> Right (s { alex_ust = ust }, ())

toString :: ByteString -> String
toString = decode . BL.unpack
}