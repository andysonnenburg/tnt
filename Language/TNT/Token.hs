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
           | Multiply
           | Div
           | Mod
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
           | PlusEqual
           | Colon
           | Semi
           | EOF deriving Show