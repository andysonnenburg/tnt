{
{-# OPTIONS_GHC -fspec-constr-count=10 #-}
module Language.Brainfuck.Parser (parse) where

import qualified Data.ByteString.Lazy as BL

import Language.Brainfuck.Command
import Language.Brainfuck.Lexer ( Alex
                                , AlexPosn (..)
                                , Token (..)
                                , alexError
                                , alexMonadScan
                                , runAlex
                                )
import qualified Language.Brainfuck.Lexer as Lexer
}

%tokentype { Token }

%token INCREMENT_POINTER { Token Lexer.IncrementPointer _ }
%token DECREMENT_POINTER { Token Lexer.DecrementPointer _ }
%token INCREMENT_BYTE { Token Lexer.IncrementByte _ }
%token DECREMENT_BYTE { Token Lexer.DecrementByte _ }
%token OUTPUT_BYTE { Token Lexer.OutputByte _ }
%token INPUT_BYTE { Token Lexer.InputByte _ }
%token JUMP_FORWARD { Token Lexer.JumpForward _ }
%token JUMP_BACKWARD { Token Lexer.JumpBackward _ }

%name parser

%monad { Alex }

%lexer { lexer } { EOF }

%error { parseError }

%%

many_commands : many_reversed_commands { reverse $1 }

many_reversed_commands : { [] }
                       | many_reversed_commands command { $2 : $1 }

command : INCREMENT_POINTER { IncrementPointer 1 }
        | DECREMENT_POINTER { IncrementPointer (-1) }
        | INCREMENT_BYTE { IncrementByte 1 } 
        | DECREMENT_BYTE { IncrementByte (-1) }
        | OUTPUT_BYTE { OutputByte }
        | INPUT_BYTE { InputByte }
        | while_nonzero { $1 }

while_nonzero : JUMP_FORWARD many_commands JUMP_BACKWARD { WhileNonzero $2 }

{
parse :: BL.ByteString -> Either String [Command]
parse = flip runAlex parser

parseError :: Token -> Alex a
parseError (Token v (AlexPn _ l c)) = alexError .
                                      showString "parse error:" .
                                      (shows l) .
                                      showChar ':' .
                                      shows c .
                                      showChar ':' .
                                      showString "unexpected " .
                                      shows v $ ""
parseError EOF = alexError "parse error: unexpected end of input"

lexer = (alexMonadScan >>=)
}
