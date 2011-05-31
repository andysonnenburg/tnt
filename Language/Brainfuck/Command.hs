module Language.Brainfuck.Command (Command (..)) where

import Data.Int

data Command = IncrementPointer Int16
             | IncrementByte Int32
             | OutputByte
             | InputByte
             | WhileNonzero [Command] deriving Show