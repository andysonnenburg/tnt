module Language.TNT.Token
       ( module Language.TNT.Position
       , Token (..)
       , Value (..)
       ) where

import Language.TNT.Position

data Token = Token Value Position
           | EOF deriving Show

data Value = Name String
           | String String
           | Import
           | Equals
           | Dot
           | OpenParen
           | CloseParen
           | Semi
           | Newline deriving Show