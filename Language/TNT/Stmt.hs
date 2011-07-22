{-# LANGUAGE
    DeriveDataTypeable
  , DeriveFunctor
  , FlexibleContexts
  , GADTs
  , StandaloneDeriving
  , TemplateHaskell
  , UndecidableInstances #-}
module Language.TNT.Stmt
       ( Dec (..)
       , Stmt (..)
       , Exp (..)
       , Property
       ) where

import Data.Data

import Language.Haskell.TH.Syntax (showName)
import qualified Language.Haskell.TH as TH

data Dec w a where
  FunD :: a -> [a] -> w (Stmt w a) -> Dec w a

deriving instance
  ( Typeable1 w
  , Typeable a
  , Data a
  , Data (w (Stmt w a))
  ) => Data (Dec w a)

instance Typeable1 w => Typeable1 (Dec w) where
  typeOf1 x = mkTyConApp tyCon [typeOf1 (getW x)]
    where
      tyCon = mkTyCon str
      str = $(return . TH.LitE . TH.StringL . showName $ ''Dec)
      getW :: Dec w a -> w a
      getW _ = undefined

data Stmt w a where
  ImportS :: w String -> w a -> Stmt w a
  DefS :: w a -> w (Exp w a) -> Stmt w a
  FunDefS :: w a -> w [w a] -> w (Stmt w a) -> Stmt w a
  IfThenS :: w (Exp w a) -> w (Stmt w a) -> Stmt w a
  IfThenElseS :: w (Exp w a) -> w (Stmt w a) -> w (Stmt w a) -> Stmt w a
  ForS :: w (Stmt w a) -> w (Exp w a) -> w (Exp w a) -> w (Stmt w a) -> Stmt w a
  ForEachS :: w a -> w (Exp w a) -> w (Stmt w a) -> Stmt w a
  WhileS :: w (Exp w a) -> w (Stmt w a) -> Stmt w a
  ReturnS :: w (Exp w a) -> Stmt w a
  ThrowS :: w (Exp w a) -> Stmt w a
  ExpS :: w (Exp w a) -> Stmt w a
  BlockS :: [w (Stmt w a)] -> Stmt w a

deriving instance Functor w => Functor (Stmt w)

deriving instance
  ( Typeable1 w
  , Typeable a
  , Data (w a)
  , Data (w String)
  , Data (w [w a])
  , Data (w (Stmt w a))
  , Data (w (Exp w a))
  ) => Data (Stmt w a)

instance Typeable1 w => Typeable1 (Stmt w) where
  typeOf1 x = mkTyConApp tyCon [typeOf1 (getW x)]
    where
      tyCon = mkTyCon str
      str = $(return . TH.LitE . TH.StringL . showName $ ''Stmt)
      getW :: Stmt w a -> w a
      getW _ = undefined

type Property w a = (w String, w (Exp w a))

data Exp w a where
  VarE :: a -> Exp w a
  NumE :: Double -> Exp w a
  StrE :: String -> Exp w a
  CharE :: Char -> Exp w a
  NullE :: Exp w a
  BoolE :: Bool -> Exp w a
  ListE :: [w (Exp w a)] -> Exp w a
  ObjE :: [w (Property w a)] -> Exp w a
  FunE :: a -> [a] -> Exp w a
  RefE :: a -> Exp w a
  RefElemE :: a -> Exp w a
  AccessE :: w (Exp w a) -> w String -> Exp w a
  MutateE :: w (Exp w a) -> w String -> w (Exp w a) -> Exp w a
  AssignE :: w a -> w (Exp w a) -> Exp w a
  AppE :: w (Exp w a) -> w [w (Exp w a)] -> Exp w a
  FunAppE :: w a -> w [w (Exp w a)] -> Exp w a
  OrE :: w (Exp w a) -> w (Exp w a) -> Exp w a
  AndE :: w (Exp w a) -> w (Exp w a) -> Exp w a

deriving instance Functor w => Functor (Exp w)

deriving instance
  ( Typeable1 w
  , Typeable a
  , Data a
  , Data (w a)
  , Data (w String)
  , Data (w (Exp w a))
  , Data (w (Property w a))
  , Data (w [w (Exp w a)])
  ) => Data (Exp w a)

instance Typeable1 w => Typeable1 (Exp w) where
  typeOf1 x = mkTyConApp tyCon [typeOf1 (getW x)]
    where
      tyCon = mkTyCon str
      str = $(return . TH.LitE . TH.StringL . showName $ ''Exp)
      getW :: Exp w a -> w a
      getW _ = undefined