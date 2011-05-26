module Language.Brainfuck.Command (Command (..)) where

data Command = IncrementPointer
             | DecrementPointer
             | IncrementByte
             | DecrementByte
             | OutputByte
             | InputByte
             | WhileNonzero [Command] deriving Show