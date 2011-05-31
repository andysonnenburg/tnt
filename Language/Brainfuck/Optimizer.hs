module Language.Brainfuck.Optimizer (optimize) where

import Language.Brainfuck.Command

optimize :: [Command] -> [Command]
optimize = reduceIncrementPointer . reduceIncrementByte

reduceIncrementPointer :: [Command] -> [Command]
reduceIncrementPointer (IncrementPointer x:IncrementPointer y:xs) =
  reduceIncrementPointer (IncrementPointer (x + y):xs)
reduceIncrementPointer (WhileNonzero xs:ys) =
  WhileNonzero (reduceIncrementPointer xs):reduceIncrementPointer ys
reduceIncrementPointer (x:xs) = x:reduceIncrementPointer xs
reduceIncrementPointer [] = []

reduceIncrementByte :: [Command] -> [Command]
reduceIncrementByte (IncrementByte x:IncrementByte y:xs) =
  reduceIncrementByte (IncrementByte (x + y):xs)
reduceIncrementByte (WhileNonzero xs:ys) =
  WhileNonzero (reduceIncrementByte xs):reduceIncrementByte ys
reduceIncrementByte (x:xs) = x:reduceIncrementByte xs
reduceIncrementByte [] = []