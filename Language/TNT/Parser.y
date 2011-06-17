{
{-# OPTIONS_GHC -fspec-constr-count=12 #-}
module Language.TNT.Parser (parse) where

import Control.Applicative

import Data.Maybe

import Language.TNT.Expression
import qualified Language.TNT.Expression as Expression
import Language.TNT.Lexer
import Language.TNT.Statement
import qualified Language.TNT.Statement as Statement
import Language.TNT.Token
import qualified Language.TNT.Token as Token
}

%tokentype { Token }

%token
  IMPORT { Token.Import }
  STRING { Token.String $$ }
  NAME { Name $$ }
  '=' { Equals }
  '.' { Dot }
  ',' { Comma }
  '(' { OpenParen }
  ')' { CloseParen }
  ';' { Semi }
  '\n' { Newline }

%name parser

%monad { Alex }

%lexer { lexer } { Token.EOF }

%error { parseError }

%%

some_statements : some_reversed_statements { reverse $ $1 }

some_reversed_statements : statement { [$1] }
                         | some_reversed_statements terminator statement {
                             $3 : $1
                           }

many_expressions : many_reversed_expressions { reverse $1 }

many_reversed_expressions : { [] }
                          | expression { [$1] }
                          | many_reversed_expressions ',' expression {
                              $3 : $1
                            }

statement : { Empty }
          | import { Statement.Import $1 }
          | expression { Expression $1 }

expression : string { $1 }
           | variable { $1 }
           | access { $1 }
           | mutate { $1 }
           | assign { $1 }
           | invoke { $1 }

terminator : ';' {}
           | '\n' {}

import : IMPORT qualified_name { $2 "" }

string : STRING { Expression.String $1 }

variable : NAME { Variable $1 }

access : expression '.' NAME { Access $1 $3 }

mutate : expression '.' NAME '=' expression { Mutate $1 $3 $5 }

assign : NAME '=' expression { Assign $1 $3 }

invoke : expression '(' many_expressions ')' {
    Invoke $1 $3
  }

qualified_name : NAME { showString $1 }
               | qualified_name '.' NAME { $1 . showChar '/' . showString $3 }

{
parse = flip runAlex parser
  
parseError _ = do
  (AlexPn _ lineNumber _, _, _) <- alexGetInput
  alexError $ show lineNumber ++ ": parse error"
  
lexer = (alexMonadScan >>=)
}
