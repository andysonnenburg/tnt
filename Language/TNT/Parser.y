{
module Language.TNT.Parser (parse) where

import Data.Maybe

import Language.TNT.Expression
import qualified Language.TNT.Expression as Expression
import Language.TNT.Lexer
import Language.TNT.Token
import qualified Language.TNT.Token as Token
}

%tokentype { Token }

%token IMPORT { Token Token.Import _ }
%token STRING { Token (Token.String $$) _ }
%token NAME { Token (Name $$) _ }
%token '=' { Token Equals _ }
%token DOT { Token Dot _ }
%token OPEN_PAREN { Token OpenParen _ }
%token CLOSE_PAREN { Token CloseParen _ }
%token SEMI { Token Semi _ }
%token NEWLINE { Token Newline _ }


%name parser

%monad { Alex }

%lexer { lexer } { Token.EOF }

%error { parseError }

%%

many_expressions : many_reversed_expressions { catMaybes . reverse $ $1 }

many_reversed_expressions : { [] }
                          | many_reversed_expressions expression { $2 : $1 }

expression : terminator { Nothing }
           | import { Just $1 }
           | string { Just $1 }
           | variable { Just $1 }
           | access { Just $1 }
           | invoke { Just $1 }

import : IMPORT optional_newline qualified_name terminator {
    Expression.Import $3
  }

string : STRING { Expression.String $1 }

variable : NAME { Variable $1 }

access : expression DOT NAME { Access $1 $3 }

mutate : expression '=' expression { Mutate $1 $3 }

invoke : expression OPEN_PAREN many_expressions CLOSE_PAREN {
    Invoke $1 $3
  }

qualified_name : NAME { $1 }

optional_newline : { () }
                 | NEWLINE { () }

terminator : NEWLINE { () }
           | SEMI { () }

{
parse = flip runAlex parser
  
parseError (Token x p) = alexError . show $ p
  
lexer = (alexMonadScan >>=)
}
