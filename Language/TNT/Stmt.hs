{-# LANGUAGE GADTs #-}
module Language.TNT.Stmt
       ( Def (..)
       , Stmt (..)
       , Exp (..)
       , Property
       ) where

import Language.TNT.Unique

data Def w a where
  Top :: Unique -> w (Stmt w a) -> Def w a
  FunDef :: String -> [a] -> [w a] -> w (Stmt w a) -> Def w a
  ObjDef :: String -> Def w a

data Stmt w a where
  ImportS :: w String -> w a -> Stmt w a
  DefS :: w a -> w (Exp w a) -> Stmt w a
  IfThenS :: w (Exp w a) -> w (Stmt w a) -> Stmt w a
  IfThenElseS :: w (Exp w a) -> w (Stmt w a) -> w (Stmt w a) -> Stmt w a
  ForS :: w (Stmt w a) -> w (Exp w a) -> w (Exp w a) -> w (Stmt w a) -> Stmt w a
  ForEachS :: w a -> w (Exp w a) -> w (Stmt w a) -> Stmt w a
  WhileS :: w (Exp w a) -> w (Stmt w a) -> Stmt w a
  ReturnS :: w (Exp w a) -> Stmt w a
  ThrowS :: w (Exp w a) -> Stmt w a
  ExpS :: w (Exp w a) -> Stmt w a
  BlockS :: [w (Stmt w a)] -> Stmt w a
                
type Property w a = (w String, w (Exp w a))

data Exp w a where
  VarE :: w a -> Exp w a
  NumE :: Double -> Exp w a
  StrE :: String -> Exp w a
  CharE :: Char -> Exp w a
  NullE :: Exp w a
  BoolE :: Bool -> Exp w a
  ListE :: [w (Exp w a)] -> Exp w a
  AccesscE :: w (Exp w a) -> w String -> Exp w a
  MutateE :: w (Exp w a) -> w String -> w (Exp w a) -> Exp w a
  AssignE :: w a -> w (Exp w a) -> Exp w a
  AppE :: w (Exp w a) -> w [w (Exp w a)] -> Exp w a
  OrE :: w (Exp w a) -> w (Exp w a) -> Exp w a
  AndE :: w (Exp w a) -> w (Exp w a) -> Exp w a