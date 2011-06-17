module Language.TNT.Token
       ( Token (..)
       ) where

data Token = Name String
           | String String
           | Import
           | Equals
           | Dot
           | Comma
           | OpenParen
           | CloseParen
           | Semi
           | Newline
           | EOF deriving Show