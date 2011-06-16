{
module Language.TNT.Parser (parse) where

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
  IMPORT { Token Token.Import _ }
  STRING { Token (Token.String $$) _ }
  NAME { Token (Name $$) _ }
  '=' { Token Equals _ }
  '.' { Token Dot _ }
  ',' { Token Comma _ }
  '(' { Token OpenParen _ }
  ')' { Token CloseParen _ }
  ';' { Token Semi _ }
  '\n' { Token Newline _ }

%name parser

%monad { Alex }

%lexer { lexer } { Token.EOF }

%error { parseError }

%%

many_statements : many_reversed_statements { reverse $ $1 }

many_reversed_statements : statement { [$1] }
                         | many_reversed_statements terminator statement {
                             $3 : $1
                           }

many_expressions : many_reversed_expressions { reverse $1 }

many_reversed_expressions : { [] }
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
           | invoke { $1 }

terminator : ';' {}
           | '\n' {}

import : IMPORT qualified_name { $2 }

string : STRING { Expression.String $1 }

variable : NAME { Variable $1 }

access : expression '.' NAME { Access $1 $3 }

mutate : expression '.' NAME '=' expression { Mutate $1 $3 $5 }

invoke : expression '(' many_expressions ')' {
    Invoke $1 $3
  }

qualified_name : NAME { $1 }

{
parse = flip runAlex parser
  
parseError x@(Token _ _) = alexError . show $ x
  
lexer = (alexMonadScan >>=)
}
