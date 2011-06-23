module Language.TNT.Token
       ( Token (..)
       ) where

data Token = Name String
           | String String
           | Import
           | As
           | Var
           | Fun
           | If
           | Else
           | For
           | In
           | Return
           | Equal
           | EQ
           | NE
           | LT
           | LE
           | GT
           | GE
           | And
           | Or
           | Dot
           | Comma
           | OpenParen
           | CloseParen
           | OpenBrace
           | CloseBrace
           | OpenBracket
           | CloseBracket
           | Semi
           | EOF deriving Show