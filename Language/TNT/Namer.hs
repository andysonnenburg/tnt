module Language.TNT.Namer (name) where

import Control.Applicative
import Control.Monad.Identity hiding (mapM)

import Data.Traversable

import Language.TNT.Error
import Language.TNT.Location
import Language.TNT.Name
import Language.TNT.Scope
import Language.TNT.Stmt

import Prelude hiding (lookup, mapM)

type Namer = ScopeT (ErrorT (Located String) Identity)

name :: Dec Located String -> Namer (Dec Located Name)
name = nameDec

nameDec :: Dec Located String -> Namer (Dec Located Name)
nameDec (FunD a b c) = do
  x <- define (a <$ c)
  y <- mapM define (map (<$ c) b)
  z <- mapM nameStmt c
  return $ FunD x y z

nameStmt :: Stmt Located String -> Namer (Stmt Located Name)
nameStmt s = case s of
  ImportS a b -> do
    x <- define b
    return $ ImportS a (x <$ b)
  DefS a b -> do
    y <- mapM nameExp b
    x <- define a
    return $ DefS (x <$ a) y
  FunDefS a b c -> do    
    x <- define a
    nestFun x $ do
      y <- mapM (mapM define') b
      z <- mapM nameStmt c
      return $ FunDefS (x <$ a) y z
  IfThenS a b ->
    IfThenS
    <$> mapM nameExp a
    <*> mapM nameStmt b
  IfThenElseS a b c ->
    IfThenElseS
    <$> mapM nameExp a
    <*> mapM nameStmt b
    <*> mapM nameStmt c
  ForS a b c d -> 
    nest $
      ForS
      <$> mapM nameStmt a
      <*> mapM nameExp b
      <*> mapM nameExp c
      <*> mapM nameStmt d
  ForEachS a b c ->
    nest $ do
      x <- define a
      y <- mapM nameExp b
      z <- mapM nameStmt c
      return (ForEachS (x <$ a) y z)
  WhileS a b ->
    WhileS
    <$> mapM nameExp a
    <*> mapM nameStmt b
  ReturnS a ->
    ReturnS
    <$> mapM nameExp a
  ThrowS a ->
    ThrowS
    <$> mapM nameExp a
  ExpS a -> do
    x <- mapM nameExp a
    return $ ExpS x
  BlockS xs ->
    nest $
      BlockS
      <$> mapM (mapM nameStmt) xs

nameExp :: Exp Located String -> Namer (Exp Located Name)
nameExp e = case e of
  VarE a -> do
    x <- lookup a
    return $ VarE (x <$ a)
  FunE a -> do
    x <- lookup a
    return $ VarE (x <$ a)
  NumE a ->
    return $ NumE a
  StrE a ->
    return $ StrE a
  CharE a ->
    return $ CharE a
  NullE ->
    return NullE
  BoolE a ->
    return $ BoolE a
  ObjE a ->
    ObjE
    <$> mapM (mapM nameProperty) a             
  ListE a ->
    ListE
    <$> mapM (mapM nameExp) a
  AccessE a b -> do
    x <- mapM nameExp a
    return $ AccessE x b
  MutateE a b c -> do
    z <- mapM nameExp c
    x <- mapM nameExp a
    return $ MutateE x b z
  AssignE a b -> do
    x <- lookup a
    y <- mapM nameExp b
    return (AssignE (x <$ a) y)
  AppE a b ->
    AppE
    <$> mapM nameExp a
    <*> mapM (mapM (mapM nameExp)) b
  FunAppE a b ->
    FunAppE
    <$> define' a
    <*> mapM (mapM (mapM nameExp)) b
  OrE a b ->
    OrE
    <$> mapM nameExp a
    <*> mapM nameExp b
  AndE a b ->
    AndE
    <$> mapM nameExp a
    <*> mapM nameExp b

define' :: Located String -> Namer (Located Name)
define' w = liftM (<$ w) (define w)

nameProperty :: Property Located String -> Namer (Property Located Name)
nameProperty (a, b) = do
  y <- mapM nameExp b
  return $ (a, y)