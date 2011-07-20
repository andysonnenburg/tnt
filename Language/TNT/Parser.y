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
  IMPORT { Locate _ Import }
  AS { Locate _ As }
  VAR { Locate _ Var }
  FUN { Locate _ Fun }
  IF { Locate _ If }
  ELSE { Locate _ Else }
  FOR { Locate _ Token.For }
  IN { Locate _ In }
  WHILE { Locate _ While }
  RETURN { Locate _ Return }
  THROW { Locate _ Throw }
  NUMBER { Locate _ (Number _) }
  STRING { Locate _ (String _) }
  CHAR { Locate _ (Char _) }
  NULL { Locate _ Null }
  NAME { Locate _ (Name _) }
  ',' { Locate _ Comma }
  '=' { Locate _ Equal }
  "+=" { Locate _ PlusEqual }
  "||" { Locate _ Or }
  "&&" { Locate _ And }
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

%monad { P }

%lexer { lexer' } { Locate _ Token.EOF }

%error { parseError }

%%

top :: { Located (Stmt Located String) }
  : some_stmts {
      BlockS . toList <%> $1
    }

some_stmts :: { Located (NonEmpty (Located (Stmt Located String))) }
  : some(stmt) { $1 }

some_exps :: { Located (NonEmpty (Located (Exp Located String))) } 
  : sepBy1(exp, ',') {
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
  | exp ';' { ExpS <%> duplicate $1 <. $2}
  | non_empty_block { $1 }

import :: { Located (Stmt Located String) }
  : IMPORT qualified_name AS NAME {
      ImportS
      <$ $1
      <.> duplicate (($ "") <%> $2)
      <. $3
      <.> duplicate (getName <%> $4)
    }

def :: { Located (Stmt Located String) }
  : VAR NAME '=' exp {
      def $1 $2 $4
    }

fun_def :: { Located (Stmt Located String) }
  : FUN NAME parameters block {
      funDef $1 $2 $3 $4
    }

if :: { Located (Stmt Located String) }
  : IF '(' exp ')' block {
      IfThenS
      <$ $1
      <.> duplicate $3
      <.> duplicate $5
    }
  | IF '(' exp ')' block ELSE if {
      IfThenElseS
      <$ $1
      <.> duplicate $3
      <.> duplicate $5
      <.> duplicate $7
    }
  | IF '(' exp ')' block ELSE block {
      IfThenElseS
      <$ $1
      <.> duplicate $3
      <.> duplicate $5
      <.> duplicate $7
    }

for :: { Located (Stmt Located String) }
  : FOR '(' def ';' exp ';' exp ')' block {
      ForS
      <$ $1
      <.> duplicate $3
      <.> duplicate $5
      <.> duplicate $7
      <.> duplicate $9
    }

for_each :: { Located (Stmt Located String) }
  : FOR '(' VAR NAME IN exp ')' block {
      ForEachS
      <$ $1
      <.> duplicate (getName <%> $4)
      <.> duplicate $6
      <.> duplicate $8
    }

while :: { Located (Stmt Located String) }
  : WHILE '(' exp ')' block {
      WhileS
      <$ $1
      <.> duplicate $3
      <.> duplicate $5
    }

return :: { Located (Stmt Located String) }
  : RETURN {
      ReturnS
      <$ $1
      <.> duplicate (NullE <$ $1)
    }
  | RETURN exp {
      ReturnS
      <$ $1
      <.> duplicate $2
    }

throw :: { Located (Stmt Located String) }
  : THROW exp {
      ThrowS
      <$ $1
      <.> duplicate $2
    }

non_empty_block :: { Located (Stmt Located String) }
  : '{' some_stmts '}' {
      BlockS . toList
      <$ $1
      <.> $2
      <. $3
    }

block :: { Located (Stmt Located String) }
  : '{' '}' {
      BlockS []
      <$ $1
      <. $2
    }
  | non_empty_block { $1 }

exp :: { Located (Exp Located String) }
  : var { $1 }
  | number { $1 }
  | string { $1 }
  | char { $1 }
  | NULL { NullE <$ $1 }
  | access { $1 }
  | mutate { $1 }
  | assign { $1 }
  | app { $1 }
  | object { $1 }
  | list { $1 }
  | exp "||" exp {
      OrE
      <%> duplicate $1
      <.> duplicate $3
    }
  | exp "&&" exp {
      AndE
      <%> duplicate $1
      <.> duplicate $3
    }
  | exp '<' exp {
      app (access $1 ("lt" <$ $2)) ((:[]) <%> duplicate $3)
    }
  | exp "<=" exp {
      app (access $1 ("le" <$ $2)) ((:[]) <%> duplicate $3)
    }
  | exp '>' exp {
      app (access $1 ("gt" <$ $2)) ((:[]) <%> duplicate $3)
    }
  | exp ">=" exp {
      app (access $1 ("ge" <$ $2)) ((:[]) <%> duplicate $3)
    }
  | exp '+' exp { plus $1 $2 $3 }
  | exp '-' exp {
      app (access $1 ("minus" <$ $2)) ((:[]) <%> duplicate $3)
    }
  | exp '*' exp {
      app (access $1 ("multiply" <$ $2)) ((:[]) <%> duplicate $3)
    }
  | exp '/' exp {
      app (access $1 ("div" <$ $2)) ((:[]) <%> duplicate $3)
    }
  | exp '%' exp {
      app (access $1 ("mod" <$ $2)) ((:[]) <%> duplicate $3)
    }
  | '!' exp {
      app (access $2 ("not" <$ $1)) ([] <$ $1)
    }
  | exp '[' exp ']' {
      app (access $1 ("get" <$ $2)) ((:[]) <%> duplicate $3) <. $4
    }
  | '(' exp ')' {
      $1 .> $2 <. $3
    }

number :: { Located (Exp Located String) }
  : NUMBER {
      NumE . getNumber <%> $1
    }

string :: { Located (Exp Located String) }
  : STRING {
      StrE . getString <%> $1
    }

char :: { Located (Exp Located String) }
  : CHAR {
      CharE . getChar <%> $1
    }

var :: { Located (Exp Located String) }
  : NAME { var $1 }

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

access :: { Located (Exp Located String) }
  : exp '.' NAME {
      access $1 (getName <%> $3)
    }

mutate :: { Located (Exp Located String) }
  : exp '.' NAME '=' exp {
      MutateE
      <%> duplicate $1
      <.> duplicate (getName <%> $3)
      <.> duplicate $5
    }

assign :: { Located (Exp Located String) }
  : NAME '=' exp {
      assign $1 $3
    }
  | NAME "+=" exp {
      assign $1 (plus (var $1) $2 $3)
    }

app :: { Located (Exp Located String) }
  : exp arguments {
      app $1 $2
    }

arguments :: { Located [Located (Exp Located String)] }
  : '(' ')' {
      []
      <$ $1
      <. $2
    }
  | '(' some_exps ')' {
      toList
      <$ $1
      <.> $2
      <. $3
    }

object :: { Located (Exp Located String) }
  : '{' '}' {
      ObjE []
      <$ $1
      <. $2
    }
  | '{' some_props '}' {
      ObjE
      <$ $1
      <.> (toList <%> $2)
      <. $3
    }

some_props :: { Located (NonEmpty (Located (Property Located String))) }
  : sepBy1(prop, ',') {
      $1 <$ sequenceA $1
    }

prop :: { Located (Property Located String) }
  : NAME ':' exp {
      (,)
      <%> duplicate (getName <%> $1)
      <.> duplicate $3
    }

list :: { Located (Exp Located String) }
  : '[' ']' {
      ListE []
      <$ $1
      <. $2
    }
  | '[' some_exps ']' {
      ListE
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
parse :: String ->
         ErrorT (Located String) Identity (Located (Stmt Located String))
parse = runP parser
  
parseError :: Located Token -> P a
parseError x = throwError $ "parse error: " ++ show (extract x) <$ x

lexer' = (lexer >>=)

getName :: Token -> String
getName (Name x) = x

getNumber :: Token -> Double
getNumber (Number x) = x

getString :: Token -> String
getString (String x) = x

getChar :: Token -> Char
getChar (Char x) = x

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
       f (Exp f String) ->
       f (Stmt f String)
def var name exp =
  DefS
  <$ var
  <.> duplicate (getName <$> name)
  <.> duplicate exp

funDef :: ( Extend f
          , Apply f
          ) =>
          f a ->
          f Token ->
          f [f String] ->
          f (Stmt f String) ->
          f (Stmt f String)
funDef fun' name parameters block =
  FunDefS
  <$ fun'
  <.> duplicate (getName <$> name)
  <.> duplicate parameters
  <.> duplicate block

assign :: ( Extend f
          , Apply f
          ) =>
          f Token ->
          f (Exp f String) ->
          f (Exp f String)
assign name exp =
  AssignE
  <%> duplicate (getName <%> name)
  <.> duplicate exp

plus :: ( Extend f
        , Apply f
        ) =>
        f (Exp f String) ->
        f a ->
        f (Exp f String) ->
        f (Exp f String)
plus x op y = app (access x ("plus" <$ op)) ((:[]) <%> duplicate y)

app :: ( Extend f
       , Apply f
       ) =>
       f (Exp f String) ->
       f [f (Exp f String)] ->
       f (Exp f String)
app x y =
  AppE
  <%> duplicate x
  <.> duplicate y

access :: ( Extend f
          , Apply f
          ) =>
          f (Exp f String) ->
          f String ->
          f (Exp f String)
access x y =
  AccessE
  <$> duplicate x
  <.> duplicate y

var :: Extend f => f Token -> f (Exp f String)
var x = VarE <$> duplicate (getName <$> x)

infixl 4 <%>

(<%>) :: Functor f => (a -> b) -> f a -> f b
(<%>) = (<$>)
}