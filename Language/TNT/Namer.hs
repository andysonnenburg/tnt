module Language.TNT.Namer (name) where

import Control.Applicative
import Control.Comonad
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
    y <- nameExp b
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
    <$> nameExp a
    <*> mapM nameStmt b
  IfThenElseS a b c ->
    IfThenElseS
    <$> nameExp a
    <*> mapM nameStmt b
    <*> mapM nameStmt c
  ForS a b c d -> 
    nest $
      ForS
      <$> mapM nameStmt a
      <*> nameExp b
      <*> nameExp c
      <*> mapM nameStmt d
  ForEachS a b c ->
    nest $ do
      x <- define a
      y <- nameExp b
      z <- mapM nameStmt c
      return (ForEachS (x <$ a) y z)
  WhileS a b ->
    WhileS
    <$> nameExp a
    <*> mapM nameStmt b
  ReturnS a ->
    ReturnS
    <$> nameExp a
  ThrowS a ->
    ThrowS
    <$> nameExp a
  ExpS a -> do
    x <- nameExp a
    return $ ExpS x
  BlockS xs ->
    nest $
      BlockS
      <$> mapM (mapM nameStmt) xs

nameExp :: Located (Exp Located String) -> Namer (Located (Exp Located Name))
nameExp e = liftM (<$ e) m
  where
    m = case (extract e) of 
      VarE a -> do
        x <- lookup (a <$ e)
        return $ VarE x
      FunE a b -> do
        x <- lookup (a <$ e)
        y <- mapM lookup (map (<$ e) b)
        return $ FunE x y
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
        <$> mapM nameExp a
      AccessE a b -> do
        x <- nameExp a
        return $ AccessE x b
      MutateE a b c -> do
        x <- nameExp a
        z <- nameExp c
        return $ MutateE x b z
      AssignE a b -> do
        x <- lookup a
        y <- nameExp b
        return (AssignE (x <$ a) y)
      AppE a b ->
        AppE
        <$> nameExp a
        <*> mapM (mapM nameExp) b
      FunAppE a b ->
        FunAppE
        <$> define' a
        <*> mapM (mapM nameExp) b
      OrE a b ->
        OrE
        <$> nameExp a
        <*> nameExp b
      AndE a b ->
        AndE
        <$> nameExp a
        <*> nameExp b

define' :: Located String -> Namer (Located Name)
define' w = liftM (<$ w) (define w)

nameProperty :: Property Located String -> Namer (Property Located Name)
nameProperty (a, b) = do
  y <- nameExp b
  return $ (a, y)