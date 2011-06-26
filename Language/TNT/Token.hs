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
           | Period
           | Comma
           | OpenParen
           | CloseParen
           | OpenBrace
           | CloseBrace
           | OpenBracket
           | CloseBracket
           | Equal
           | Semi
           | EOF deriving Show