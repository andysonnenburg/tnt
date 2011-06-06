module Language.Brainfuck.Optimizer (optimize) where

import Language.Brainfuck.Command

optimize :: [Command] -> [Command]
optimize = id