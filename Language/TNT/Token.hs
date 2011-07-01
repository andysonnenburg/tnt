module Language.TNT.Token
       ( Token (..)
       ) where

data Token = Name String
           | Operator String
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
           | Return
           | Throw
           | LT
           | LE
           | GT
           | GE
           | Not
           | Or
           | Plus
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