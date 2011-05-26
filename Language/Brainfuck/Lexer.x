{
{-# OPTIONS_GHC
    -fno-warn-lazy-unlifted-bindings
    -fno-warn-name-shadowing
    -fno-warn-unused-binds
    -fno-warn-unused-matches #-}
module Language.Brainfuck.Lexer
       ( Alex
       , AlexPosn (..)
       , Token (..)
       , Value (..)
       , alexError
       , alexMonadScan
       , runAlex
       ) where
}

%wrapper "monad-bytestring"

:-
  ">" { token' IncrementPointer }
  "<" { token' DecrementPointer }
  "+" { token' IncrementByte }
  "-" { token' DecrementByte }
  "." { token' OutputByte }
  "," { token' InputByte }
  "[" { token' JumpForward }
  "]" { token' JumpBackward }
  .|\n ;

{

data Token = Token Value AlexPosn
           | EOF

data Value = IncrementPointer
           | DecrementPointer
           | IncrementByte
           | DecrementByte
           | OutputByte
           | InputByte
           | JumpForward
           | JumpBackward deriving Show

token' v (p, _, _) _ = return $ Token v p

alexEOF = return EOF
}