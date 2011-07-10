{-# LANGUAGE ScopedTypeVariables #-}
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

name :: Top Located String ->
        ErrorT (Located String) Identity (Top Located Name)
name (Top a b) = flip runScopeT a $ do
  x <- mapM nameStmt' b
  return $ Top a x
  where
    nameStmt' w = do
      x <- nameStmt (extract w)
      return (x <$ w)

nameStmt :: Stmt Located String ->
            Namer (Stmt Located Name)
nameStmt s = case s of
  Import a b -> do
    x <- define b
    return $ Import a (x <$ b)
  Decl a b -> do
    y <- mapM nameExpr b
    x <- define a
    return $ Decl (x <$ a) y
  IfThen a b ->
    IfThen
    <$> mapM nameExpr a
    <*> mapM nameStmt b
  IfThenElse a b c ->
    IfThenElse
    <$> mapM nameExpr a
    <*> mapM nameStmt b
    <*> mapM nameStmt c
  For a b c d -> 
    nest $
      For
      <$> mapM nameStmt a
      <*> mapM nameExpr b
      <*> mapM nameExpr c
      <*> mapM nameStmt d
  ForEach a b c ->
    nest $ do
      x <- define a
      y <- mapM nameExpr b
      z <- mapM nameStmt c
      return (ForEach (x <$ a) y z)
  While a b ->
    While
    <$> mapM nameExpr a
    <*> mapM nameStmt b
  Return a ->
    Return
    <$> mapM nameExpr a
  Throw a ->
    Throw
    <$> mapM nameExpr a
  Expr a -> do
    x <- mapM nameExpr a
    return $ Expr x
  Block xs ->
    nest $
      Block
      <$> mapM (mapM nameStmt) xs

nameExpr :: Expr Located String ->
            Namer (Expr Located Name)
nameExpr e = case e of
  Var a -> do
    x <- lookup a
    return $ Var (x <$ a)
  Fun a b c ->
    nestFun a $ do
      let define' x = liftM (<$ x) (define x)
      x <- mapM (mapM define') b
      y <- mapM nameStmt c
      return (Fun a x y)
  Number a ->
    return $ Number a
  String a ->
    return $ String a
  Char a ->
    return $ Char a
  Null ->
    return Null
  Bool a ->
    return $ Bool a
  Object a ->
    Object
    <$> mapM (mapM nameProperty) a             
  List a ->
    List
    <$> mapM (mapM nameExpr) a
  Access a b -> do
    x <- mapM nameExpr a
    return $ Access x b
  Mutate a b c -> do
    z <- mapM nameExpr c
    x <- mapM nameExpr a
    return $ Mutate x b z
  Assign a b -> do
    x <- lookup a
    y <- mapM nameExpr b
    return (Assign (x <$ a) y)
  App a b ->
    App
    <$> mapM nameExpr a
    <*> mapM (mapM (mapM nameExpr)) b
  Or a b ->
    Or
    <$> mapM nameExpr a
    <*> mapM nameExpr b
  And a b ->
    And
    <$> mapM nameExpr a
    <*> mapM nameExpr b

nameProperty :: Property Located String ->
                Namer (Property Located Name)
nameProperty (a, b) = do
  y <- mapM nameExpr b
  return $ (a, y)