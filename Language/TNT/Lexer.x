{
{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , NamedFieldPuns
  , RecordWildCards
  , StandaloneDeriving #-}
module Language.TNT.Lexer
       ( P
       , runP
       , lexer
       , lex
       ) where

import Control.Applicative
import Control.Monad.Error.Class
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer

import Data.Bits
import Data.Char
import Data.Semigroup

import Language.TNT.Location
import Language.TNT.Message
import Language.TNT.Token

import Prelude hiding (Ordering (..), getChar, last, lex)
}

@name = [a-zA-Z_] [a-zA-Z_0-9]*
@digit = [0-9]

tnt :-

$white+ ;

"//".* ;

<0> {
  \< { special LT }
  "<=" { special LE }
  \> { special GT }
  ">=" { special GE }
  "||" { special Or }
  "&&" { special And }
  \+ { special Plus }
  \- { special Minus }
  \* { special Multiply }
  \/ { special Div }
  \% { special Mod }
  \! { special Not }
  \( { special OpenParen }
  \) { special CloseParen }
  \[ { special OpenBracket }
  \] { special CloseBracket }
  \, { special Comma }
  \{ { special OpenBrace }
  \} { special CloseBrace }
  \. { special Period }
  \= { special Equal }
  \: { special Colon }
  \; { special Semi }
  "import" { special Import }
  "as" { special As }
  "var" { special Var }
  "fun" { special Fun }
  "if" { special If }
  "elif" { special Elif }
  "else" { special Else }
  "for" { special For }
  "in" { special In }
  "while" { special While }
  "return" { special Return }
  "throw" { special Throw }
  @name { name }
  @digit { integral }
  \' { char }
  \" { string }
}

{

type Action = Location -> String -> Int -> P (Located Token)

special :: Token -> Action
special token l _ _ = return (Locate l token)

name :: Action
name l s n = return . Locate l . Name . take n $ s

char :: Action
char first _ _ = do
  c <- getChar
  case c of
    '\'' -> do
      last <- getPoint
      let l = first <> Location last last
      throwError $ Message l  "lexical error"
    '\\' -> do
      c' <- getEscapedChar
      '\'' <- getChar
      last <- getPoint
      let l = first <> Location last last
      return $ Locate l (Char c')
    _ -> do
      '\'' <- getChar
      last <- getPoint
      let l = first <> Location last last
      return $ Locate l (Char c)

string :: Action
string first _ _ = do
  s <- string' ""
  last <- getPoint
  let l = first <> Location last last
  return $ Locate l s

string' :: String -> P Token
string' s = do
  c <- getChar
  case c of
    '"' -> return . String . reverse $ s
    '\\' -> do
      c' <- getEscapedChar
      string' (c':s)
    _ -> string' (c:s)

integral :: Action
integral l _ _ = do
  return $ Locate l (Number 0)

getChar :: P Char
getChar = do
  i <- getInput
  case alexGetChar i of
    Nothing -> fail "lexical error"
    Just (c, i') -> do
      setInput i'
      return c

getEscapedChar :: P Char
getEscapedChar = do
  first <- getPoint
  c <- getChar
  case c of
    'n' -> return '\n'
    _ -> do
      last <- getPoint
      let l = Location first last
      throwError $ Message l "lexical error"

getInput :: P AlexInput
getInput = P $ do
  S {..} <- get
  return $ AI point buffer

setInput :: AlexInput -> P ()
setInput (AI point buffer) = P $ do
  s <- get
  put s { point, buffer }

getPoint :: P Point
getPoint = P $ do
  S {..} <- get
  return point

getStartCode :: P Int
getStartCode = P $ do
  S {..} <- get
  return startCode

data S = S
         { point :: Point
         , buffer :: String
         , startCode :: Int
         }

newtype ErrorT e m a = ErrorT
                       { unErrorT :: m (Either e a)
                       }

instance Functor m => Functor (ErrorT e m) where
  fmap f = ErrorT . fmap (fmap f) . unErrorT

instance Monad m => Monad (ErrorT e m) where
  return = ErrorT . return . return
  ErrorT m >>= k = ErrorT $ do
    a <- m
    case a of
      Left l -> return . Left $ l
      Right r -> unErrorT (k r)

instance Monad m => MonadError e (ErrorT e m) where
  throwError = ErrorT . return . Left
  ErrorT m `catchError` h = ErrorT $ do
    a <- m
    case a of
      Left e -> unErrorT (h e)
      r@(Right _) -> return r

newtype P a = P
              { unP :: StateT S (ErrorT Message Identity) a
              } deriving ( Functor
                         , Applicative
                         )

deriving instance MonadError Message P

instance Monad P where
  return = P . return
  (P m) >>= k = P $ m >>= unP . k
  fail msg = P $ do
    S {..} <- get
    throwError $ Message (Location point point) msg

runP :: P a -> String -> Either Message a
runP (P m) buffer = runIdentity m''
  where
    s = S { point = Point 1 0
          , buffer
          , startCode = 0
          }
    m' = evalStateT m s
    m'' = unErrorT m'

lexer :: P (Located Token)
lexer = do
  i@(AI p b) <- getInput
  sc <- getStartCode
  case alexScan i sc of
    AlexEOF -> return $ Locate (Location p p) EOF
    AlexError (AI p' _) -> 
      throwError $ Message (Location p p') "lexical error"
    AlexSkip i' _ -> do
      setInput i'
      lexer
    AlexToken i'@(AI p' _) n m -> do
      setInput i'
      let l = Location p p'
      m l b n

lex :: String -> Either Message [Located Token]
lex = runP lex'

lex' :: P [Located Token]
lex' = do
  x <- lexer
  case x of
    Locate _ EOF ->
      return []
    _ -> do
      xs <- lex'
      return (x:xs)

data AlexInput = AI Point String

alexGetChar :: AlexInput -> Maybe (Char, AlexInput)
alexGetChar (AI p s) = case s of
  (c:s') -> Just (c, AI (movePoint p c) s')
  [] -> Nothing

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = undefined

movePoint :: Point -> Char -> Point
movePoint (Point y x) c =
  case c of
    '\n' -> Point (y + 1) 0
    '\t' -> Point y (((((x - 1) `shiftR` 3) + 1) `shiftL` 3) + 1)
    _ -> Point y (x + 1)
}