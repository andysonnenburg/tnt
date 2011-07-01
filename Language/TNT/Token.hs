module Language.TNT.Token
       ( Token (..)
       ) where

data Token = Name String
           | Operator String
           | Number Double
           | String String
           | Char Char
           | Import
           | As
           | Var
           | Fun
           | If
           | Elif
           | Else
           | For
           | In
           | While
           | Return
           | Throw
           | Or
           | And
           | LT
           | LE
           | GT
           | GE
           | Plus
           | Minus
           | Not
           | Period
           | Comma
           | OpenParen
           | CloseParen
           | OpenBrace
           | CloseBrace
           | OpenBracket
           | CloseBracket
           | Equal
           | Colon
           | Semi
           | EOF deriving Show