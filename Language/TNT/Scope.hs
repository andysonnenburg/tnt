{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , RecordWildCards
  , StandaloneDeriving
  , TypeSynonymInstances
  , UndecidableInstances #-}
module Language.TNT.Scope
       ( ScopeT
       , runScopeT
       , define
       , lookup
       , nest
       , nestFun
       ) where

import Control.Applicative
import Control.Comonad
import Control.Monad.Error
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Language.TNT.Location
import Language.TNT.Name
import Language.TNT.Unique

import Prelude hiding (lookup)

data S = S { currentScopeName :: Unique
           , currentScope :: Map String Name
           , scopeChain :: [Map String Name]
           }

newtype ScopeT m a = ScopeT
                     { unScopeT :: StateT S (UniqueT m) a
                     } deriving ( Functor
                                , Applicative
                                , Monad
                                )

deriving instance MonadError e m => MonadError e (ScopeT m)

instance MonadState s m => MonadState s (ScopeT m) where
  get = ScopeT . lift . lift $ get
  put = ScopeT . lift . lift . put

instance MonadTrans ScopeT where
  lift = ScopeT . lift . lift

getCurrentScopeName :: Monad m => ScopeT m Unique
getCurrentScopeName = ScopeT $ do
  S {..} <- get
  return currentScopeName

getCurrentScope :: Monad m => ScopeT m (Map String Name)
getCurrentScope = ScopeT $ do
  S {..} <- get
  return currentScope

putCurrentScope :: Monad m => Map String Name -> ScopeT m ()
putCurrentScope m = ScopeT $ do
  s@S {..} <- get
  put s { currentScope = m }

getScopeChain :: Monad m => ScopeT m [Map String Name]
getScopeChain = ScopeT $ do
  S {..} <- get
  return scopeChain

runScopeT :: Monad m => ScopeT m a -> Unique -> m a
runScopeT (ScopeT m) x = runUniqueT $ evalStateT m s
  where
    s = S { currentScopeName = x
          , currentScope = Map.empty
          , scopeChain = []
          }

define :: MonadError (Located String) m => Located String -> ScopeT m Name
define w = do
  m <- getCurrentScope
  case Map.lookup s m of
    Nothing -> do
      x <- newName s
      let m' = Map.insert s x m
      putCurrentScope m'
      return x
    Just _ ->
      throwError $ ("scope error: " ++ show s ++ " already defined") <$ w
  where
    s = extract w

newName :: Monad m => String -> ScopeT m Name
newName s = do
  x <- getCurrentScopeName
  y <- ScopeT (lift newUnique)
  return $ Name x y s

lookup :: MonadError (Located String) m => Located String -> ScopeT m Name
lookup w = do
  m <- getCurrentScope
  case Map.lookup (extract w) m of
    Just a ->
      return a
    Nothing -> do
      ms <- getScopeChain
      lookup' w ms

lookup' :: MonadError (Located String) m =>
           Located String -> [Map String Name] -> ScopeT m Name
lookup' w xs =
  case xs of
    y:ys ->
      case Map.lookup s y of
        Just a -> 
          return a
        Nothing ->
          lookup' w ys
    [] -> throwError $ ("scope error: " ++ show s ++ " not defined") <$ w
  where
    s = extract w

nest :: Monad m => ScopeT m a -> ScopeT m a
nest m = do
  x <- getCurrentScopeName
  nestFun x m

nestFun :: Monad m => Unique -> ScopeT m a -> ScopeT m a
nestFun x m = ScopeT $ do
  s@S {..} <- get
  let s' = S { currentScopeName = x
             , currentScope = Map.empty
             , scopeChain = currentScope:scopeChain
             }
  put s'
  a <- unScopeT $ m
  put s
  return a
