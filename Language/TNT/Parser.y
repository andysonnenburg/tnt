{
{-# OPTIONS_GHC -fspec-constr-count=21 #-}
module Language.TNT.Parser (parse) where

import Control.Applicative

import Data.Maybe

import Language.TNT.Lexer
import Language.TNT.Stmt as Stmt
import Language.TNT.Token as Token

import Prelude hiding (Ordering (..))
}

%tokentype { Token }

%token
  IMPORT { Token.Import }
  AS { As }
  VAR { Token.Var }
  FUN { Token.Fun }
  IF { If }
  ELSE { Else }
  FOR { Token.For }
  IN { In }
  RETURN { Token.Return }
  STRING { Token.String $$ }
  NAME { Name $$ }
  '=' { Equal }
  "==" { EQ }
  "!=" { NE }
  '<' { LT }
  "<=" { LE }
  '>' { GT }
  ">=" { GE }
  '.' { Dot }
  ',' { Comma }
  '(' { OpenParen }
  ')' { CloseParen }
  '{' { OpenBrace }
  '}' { CloseBrace }
  '[' { OpenBracket }
  ']' { CloseBracket }
  ';' { Semi }

%name parser

%monad { Alex }

%lexer { lexer } { Token.EOF }

%error { parseError }

%%

many_stmts :: { [Stmt String] }
  : many_reversed_stmts { reverse $1 }

many_reversed_stmts :: { [Stmt String] }
  : { [] }
  | many_reversed_stmts stmt {
      $2 : $1
    }

many_exprs :: { [Expr String] } 
  : many_reversed_exprs { reverse $1 }

many_reversed_exprs :: { [Expr String] } 
  : { [] }
  | expr { [$1] }
  | many_reversed_exprs ',' expr {
      $3 : $1
    }

many_names :: { [String] }
  : many_reversed_names { reverse $1 }

many_reversed_names :: { [String] }
  : { [] }
  | NAME { [$1] }
  | many_reversed_names ',' NAME { $3 : $1 }

stmt :: { Stmt String }
  : import ';' { $1 }
  | decl ';' { $1 }
  | if { $1 }
  | for { $1 }
  | forEach { $1 }
  | return ';' { $1 }
  | expr ';' { Expr $1 }

import :: { Stmt String }
  : IMPORT split_qualified_name { Stmt.Import (fst $2) (snd $2) }
  | IMPORT qualified_name AS NAME { Stmt.Import ($2 "") $4 }

decl :: { Stmt String }
  : VAR NAME '=' expr { Decl $2 $4 }
  | FUN NAME '(' many_names ')' '{' many_stmts '}' { Decl $2 (Stmt.Fun $4 $7) }

if :: { Stmt String }
  : IF '(' expr ')' '{' many_stmts '}' { IfThen $3 $6 }
  | IF '(' expr ')' '{' many_stmts '}' ELSE '{' many_stmts '}' {
      IfThenElse $3 $6 $10
    }

for :: { Stmt String }
  : FOR '(' stmt ';' expr ';' expr ')' '{' many_stmts '}' {
      Stmt.For $3 $5 $7 $10
    }

forEach :: { Stmt String }
  : FOR '(' NAME IN expr ')' '{' many_stmts '}' {
      Stmt.ForEach $3 $5 $8
    }

return :: { Stmt String }
  : RETURN expr { Stmt.Return $2 }

expr :: { Expr String }
  : var { $1 }
  | fun { $1 }
  | string { $1 }
  | access { $1 }
  | mutate { $1 }
  | assign { $1 }
  | app { $1 }
  | list { $1 }
  | compare { $1 }

string
  : STRING { Stmt.String $1 }

var
  : NAME { Stmt.Var $1 }

fun
  : FUN '(' many_names ')' '{' many_stmts '}' { Stmt.Fun $3 $6 }

access
  : expr '.' NAME { Access $1 $3 }

mutate
  : expr '.' NAME '=' expr { Mutate $1 $3 $5 }

assign
  : NAME '=' expr { Assign $1 $3 }

app
  : expr '(' many_exprs ')' {
    App $1 $3
  }

list
  : '[' many_exprs ']' { List $2 }

compare
  : expr compareOperator expr { App (Access $1 $2) [$3] }

compareOperator
  : "==" { "eq" }
  | "!=" { "ne" }
  | '<' { "lt" }
  | "<=" { "le" }
  | '>' { "gt" }
  | ">=" { "ge" }

qualified_name : NAME { showString $1 }
               | qualified_name '.' NAME { $1 . showChar '/' . showString $3 }

split_qualified_name
  : NAME { ($1, $1) }
  | qualified_name '.' NAME { ($1 . showChar '/' . showString $3 $ "", $3) }

{
parse = flip runAlex parser
  
parseError x = do
   (AlexPn _ l _, _, _) <- alexGetInput
   alexError $ show l ++ ": unexpected " ++ show x
  
lexer = (alexMonadScan' >>=)
}