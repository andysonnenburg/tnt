{
{-# OPTIONS_GHC -fspec-constr-count=23 #-}
module Language.TNT.Parser (parse) where

import Control.Comonad
import Control.Monad.Error.Class
import Control.Monad.Identity hiding (mapM)
import Control.Monad.Reader hiding (mapM)
import Control.Monad.Trans

import Data.Foldable (foldr1)
import Data.Functor.Apply
import Data.List.NonEmpty
import Data.Maybe
import Data.Semigroup
import Data.Traversable hiding (sequenceA)

import Language.TNT.Error
import Language.TNT.Lexer
import Language.TNT.Location
import Language.TNT.Stmt as Stmt
import Language.TNT.Token hiding (False)
import qualified Language.TNT.Token as Token
import Language.TNT.Unique

import Prelude hiding (Ordering (..), foldr1, getChar, mapM, reverse)
import qualified Prelude
}

%tokentype { Located Token }

%token
  IMPORT { Locate _ Token.Import }
  AS { Locate _ As }
  VAR { Locate _ Token.Var }
  FUN { Locate _ Token.Fun }
  IF { Locate _ If }
  ELSE { Locate _ Else }
  FOR { Locate _ Token.For }
  IN { Locate _ In }
  WHILE { Locate _ Token.While }
  RETURN { Locate _ Token.Return }
  THROW { Locate _ Token.Throw }
  NUMBER { Locate _ (Token.Number _) }
  STRING { Locate _ (Token.String _) }
  CHAR { Locate _ (Token.Char _) }
  NULL { Locate _ Token.Null }
  NAME { Locate _ (Name _) }
  ',' { Locate _ Comma }
  '=' { Locate _ Equal }
  "+=" { Locate _ PlusEqual }
  "||" { Locate _ Token.Or }
  "&&" { Locate _ Token.And }
  '<' { Locate _ LT }
  "<=" { Locate _ LE }
  '>' { Locate _ GT }
  ">=" { Locate _ GE }
  '+' { Locate _ Plus }
  '-' { Locate _ Minus }
  '*' { Locate _ Multiply }
  '/' { Locate _ Div }
  '%' { Locate _ Mod }
  '!' { Locate _ Not }
  '.' { Locate _ Period }
  '(' { Locate _ OpenParen }
  ')' { Locate _ CloseParen }
  '{' { Locate _ OpenBrace }
  '}' { Locate _ CloseBrace }
  '[' { Locate _ OpenBracket }
  ']' { Locate _ CloseBracket }
  ':' { Locate _ Colon }
  ';' { Locate _ Semi }

%left ','
%right '=' "+="
%left "||"
%left "&&"
%left "==" "!="
%left '<' "<=" '>' ">="
%left '+' '-'
%left '*' '/' '%'
%right '!'
%left '(' ')' '[' ']' '.'

%name parser

%monad { UniqueT P }

%lexer { lexer' } { Locate _ Token.EOF }

%error { parseError }

%%

top :: { Def Located String }
  : some_stmts {% do
      x <- newUnique
      return $ Top x (Block . toList <%> $1)
    }

some_stmts :: { Located (NonEmpty (Located (Stmt Located String))) }
  : some(stmt) { $1 }

some_exprs :: { Located (NonEmpty (Located (Expr Located String))) } 
  : sepBy1(expr, ',') {
      $1 <$ sequenceA $1
    }

some_names :: { Located (NonEmpty (Located String)) }
  : some_reversed_names {
      let names = reverse $1
      in names <$ sequenceA names
    }

some_reversed_names :: { NonEmpty (Located String) }
  : NAME {
      (getName <%> $1) :| []
    }
  | some_reversed_names ',' NAME {
      (getName <%> $3) <| $1
    }

stmt :: { Located (Stmt Located String) }
  : import ';' { $1 <. $2 }
  | def ';' { $1 <. $2 }
  | fun_def { $1 }
  | if { $1 }
  | for { $1 }
  | for_each { $1 }
  | while { $1 }
  | return ';' { $1 <. $2 }
  | throw ';' { $1 <. $2 }
  | expr ';' { Expr <%> duplicate $1 <. $2}
  | non_empty_block { $1 }

import :: { Located (Stmt Located String) }
  : IMPORT qualified_name AS NAME {
      Stmt.Import
      <$ $1
      <.> duplicate (($ "") <%> $2)
      <. $3
      <.> duplicate (getName <%> $4)
    }

def :: { Located (Stmt Located String) }
  : VAR NAME '=' expr {
      def $1 $2 $4
    }

fun_def :: { Located (Stmt Located String) }
  : FUN NAME parameters block {%
      def $1 $2 <%> fun $1 $3 $4
    }

if :: { Located (Stmt Located String) }
  : IF '(' expr ')' block {
      IfThen
      <$ $1
      <.> duplicate $3
      <.> duplicate $5
    }
  | IF '(' expr ')' block ELSE if {
      IfThenElse
      <$ $1
      <.> duplicate $3
      <.> duplicate $5
      <.> duplicate $7
    }
  | IF '(' expr ')' block ELSE block {
      IfThenElse
      <$ $1
      <.> duplicate $3
      <.> duplicate $5
      <.> duplicate $7
    }

for :: { Located (Stmt Located String) }
  : FOR '(' def ';' expr ';' expr ')' block {
      Stmt.For
      <$ $1
      <.> duplicate $3
      <.> duplicate $5
      <.> duplicate $7
      <.> duplicate $9
    }

for_each :: { Located (Stmt Located String) }
  : FOR '(' VAR NAME IN expr ')' block {
      ForEach
      <$ $1
      <.> duplicate (getName <%> $4)
      <.> duplicate $6
      <.> duplicate $8
    }

while :: { Located (Stmt Located String) }
  : WHILE '(' expr ')' block {
      Stmt.While
      <$ $1
      <.> duplicate $3
      <.> duplicate $5
    }

return :: { Located (Stmt Located String) }
  : RETURN {
      Stmt.Return
      <$ $1
      <.> duplicate (Stmt.Null <$ $1)
    }
  | RETURN expr {
      Stmt.Return
      <$ $1
      <.> duplicate $2
    }

throw :: { Located (Stmt Located String) }
  : THROW expr {
      Stmt.Throw
      <$ $1
      <.> duplicate $2
    }

non_empty_block :: { Located (Stmt Located String) }
  : '{' some_stmts '}' {
      Block . toList
      <$ $1
      <.> $2
      <. $3
    }

block :: { Located (Stmt Located String) }
  : '{' '}' {
      Block []
      <$ $1
      <. $2
    }
  | non_empty_block { $1 }

expr :: { Located (Expr Located String) }
  : var { $1 }
  | fun { $1 }
  | number { $1 }
  | string { $1 }
  | char { $1 }
  | NULL { Stmt.Null <$ $1 }
  | access { $1 }
  | mutate { $1 }
  | assign { $1 }
  | app { $1 }
  | object { $1 }
  | list { $1 }
  | expr "||" expr {
      Stmt.Or
      <%> duplicate $1
      <.> duplicate $3
    }
  | expr "&&" expr {
      Stmt.And
      <%> duplicate $1
      <.> duplicate $3
    }
  | expr '<' expr {
      app (access $1 ("lt" <$ $2)) ((:[]) <%> duplicate $3)
    }
  | expr "<=" expr {
      app (access $1 ("le" <$ $2)) ((:[]) <%> duplicate $3)
    }
  | expr '>' expr {
      app (access $1 ("gt" <$ $2)) ((:[]) <%> duplicate $3)
    }
  | expr ">=" expr {
      app (access $1 ("ge" <$ $2)) ((:[]) <%> duplicate $3)
    }
  | expr '+' expr { plus $1 $2 $3 }
  | expr '-' expr {
      app (access $1 ("minus" <$ $2)) ((:[]) <%> duplicate $3)
    }
  | expr '*' expr {
      app (access $1 ("multiply" <$ $2)) ((:[]) <%> duplicate $3)
    }
  | expr '/' expr {
      app (access $1 ("div" <$ $2)) ((:[]) <%> duplicate $3)
    }
  | expr '%' expr {
      app (access $1 ("mod" <$ $2)) ((:[]) <%> duplicate $3)
    }
  | '!' expr {
      app (access $2 ("not" <$ $1)) ([] <$ $1)
    }
  | expr '[' expr ']' {
      app (access $1 ("get" <$ $2)) ((:[]) <%> duplicate $3) <. $4
    }
  | '(' expr ')' {
      $1 .> $2 <. $3
    }

number :: { Located (Expr Located String) }
  : NUMBER {
      Stmt.Number . getNumber <%> $1
    }

string :: { Located (Expr Located String) }
  : STRING {
      Stmt.String . getString <%> $1
    }

char :: { Located (Expr Located String) }
  : CHAR {
      Stmt.Char . getChar <%> $1
    }

var :: { Located (Expr Located String) }
  : NAME { var $1 }

fun :: { Located (Expr Located String) }
  : FUN parameters block {% fun $1 $2 $3 }

parameters :: { Located [Located String] }
  : '(' ')' {
      []
      <$ $1
      <. $2
    }
  | '(' some_names ')' {
      toList
      <$ $1
      <.> $2
      <. $3
    }

access :: { Located (Expr Located String) }
  : expr '.' NAME {
      access $1 (getName <%> $3)
    }

mutate :: { Located (Expr Located String) }
  : expr '.' NAME '=' expr {
      Mutate
      <%> duplicate $1
      <.> duplicate (getName <%> $3)
      <.> duplicate $5
    }

assign :: { Located (Expr Located String) }
  : NAME '=' expr {
      assign $1 $3
    }
  | NAME "+=" expr {
      assign $1 (plus (var $1) $2 $3)
    }

app :: { Located (Expr Located String) }
  : expr arguments {
      app $1 $2
    }

arguments :: { Located [Located (Expr Located String)] }
  : '(' ')' {
      []
      <$ $1
      <. $2
    }
  | '(' some_exprs ')' {
      toList
      <$ $1
      <.> $2
      <. $3
    }

object :: { Located (Expr Located String) }
  : '{' '}' {
      AnonObj []
      <$ $1
      <. $2
    }
  | '{' some_props '}' {
      AnonObj
      <$ $1
      <.> (toList <%> $2)
      <. $3
    }

some_props :: { Located (NonEmpty (Located (Property Located String))) }
  : sepBy1(prop, ',') {
      $1 <$ sequenceA $1
    }

prop :: { Located (Property Located String) }
  : NAME ':' expr {
      (,)
      <%> duplicate (getName <%> $1)
      <.> duplicate $3
    }

list :: { Located (Expr Located String) }
  : '[' ']' {
      List []
      <$ $1
      <. $2
    }
  | '[' some_exprs ']' {
      List
      <$ $1
      <.> (toList <%> $2)
      <. $3
    }

qualified_name :: { Located (String -> String) }
  : NAME {
      showString . getName <%> $1
    }
  | qualified_name '.' NAME {
      (.)
      <%> $1
      <.> ((showChar '/' .) . showString . getName <%> $3)
    }

sepBy1(p, sep) :: { NonEmpty a }
  : p many(snd(sep, p)) {
      $1 :| $2
    }

snd(p, q) :: { a }
  : p q { $2 }

many(p) :: { [a] }
  : many_reversed(p) {
      Prelude.reverse $1
    }

many_reversed(p) :: { [a] }
  : { [] }
  | many_reversed(p) p {
      $2 : $1
    }

some(p)
  : some_reversed(p) {
      let xs = reverse $1
      in xs <$ sequenceA xs
    }

some_reversed(p)
  : p {
      $1 :| []
    }
  | some_reversed(p) p {
      $2 <| $1
    }

{
parse :: String -> ErrorT (Located String) Identity (Unique, Located (Stmt Located String))
parse = runP . runUniqueT $ parser
  
parseError :: Located Token -> UniqueT P a
parseError x = throwError $ "parse error: " ++ show (extract x) <$ x

lexer' = (lift lexer >>=)

getName :: Token -> String
getName (Name x) = x

getNumber :: Token -> Double
getNumber (Token.Number x) = x

getString :: Token -> String
getString (Token.String x) = x

getChar :: Token -> Char
getChar (Token.Char x) = x

sequenceA :: Apply f => NonEmpty (f a) -> f (NonEmpty a)
sequenceA ~(x :| xs) = go x xs
  where
    go y [] = (:| []) <$> y
    go y (z:zs) = (<|) <$> y <.> go z zs

def :: ( Extend f
       , Apply f
       ) =>
       f a ->
       f Token ->
       f (Expr f String) ->
       f (Stmt f String)
def var name expr =
  Def
  <$ var
  <.> duplicate (getName <$> name)
  <.> duplicate expr

assign :: ( Extend f
          , Apply f
          ) =>
          f Token ->
          f (Expr f String) ->
          f (Expr f String)
assign name expr =
  Assign
  <%> duplicate (getName <%> name)
  <.> duplicate expr

plus :: ( Extend f
        , Apply f
        ) =>
        f (Expr f String) ->
        f a ->
        f (Expr f String) ->
        f (Expr f String)
plus x op y = app (access x ("plus" <$ op)) ((:[]) <%> duplicate y)

app :: ( Extend f
       , Apply f
       ) =>
       f (Expr f String) ->
       f [f (Expr f String)] ->
       f (Expr f String)
app x y =
  App
  <%> duplicate x
  <.> duplicate y

access :: ( Extend f
          , Apply f
          ) =>
          f (Expr f String) ->
          f String ->
          f (Expr f String)
access x y =
  Access
  <$> duplicate x
  <.> duplicate y

var :: Extend f => f Token -> f (Expr f String)
var x = Stmt.Var <$> duplicate (getName <$> x)

fun :: ( Extend f
       , Apply f
       ) =>
       f a ->
       f [f String] ->
       f (Stmt f String) ->
       UniqueT P (f (Expr f String))
fun fun' parameters block = do
  x <- newUnique
  return $
    AnonFun x
    <$ fun'
    <.> duplicate parameters
    <.> duplicate block

infixl 4 <%>

(<%>) :: Functor f => (a -> b) -> f a -> f b
(<%>) = (<$>)
}