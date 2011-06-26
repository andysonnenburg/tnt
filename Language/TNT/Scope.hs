{-# LANGUAGE
    FlexibleContexts
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , RecordWildCards
  , StandaloneDeriving
  , TypeSynonymInstances #-}
module Language.TNT.Scope
       ( ScopeT
       , runScopeT
       , define
       , lookup
       , withScope
       ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map

import Prelude hiding (lookup)

data S a = S { currentScope :: Map String a
             , scopeChain :: [Map String a]
             }

newtype ScopeT v m a = ScopeT
                       { unScopeT :: StateT (S v) m a
                       } deriving ( Functor
                                  , Applicative
                                  , Monad
                                  )

deriving instance MonadError String m => MonadError String (ScopeT v m)

getCurrentScope :: Monad m => ScopeT v m (Map String v)
getCurrentScope = ScopeT $ do
  S {..} <- get
  return currentScope

putCurrentScope :: Monad m => Map String v -> ScopeT v m ()
putCurrentScope m = ScopeT $ do
  s@S {..} <- get
  put s { currentScope = m }

getScopeChain :: Monad m => ScopeT v m [Map String v]
getScopeChain = ScopeT $ do
  S {..} <- get
  return scopeChain

runScopeT :: Monad m => ScopeT v m a -> m a
runScopeT = flip evalStateT s . unScopeT
  where
    s = S { currentScope = Map.empty
          , scopeChain = []
          }

define :: MonadError String m => String -> v -> ScopeT v m ()
define s a = do
  m <- getCurrentScope
  case Map.lookup s m of
    Nothing -> do
      let m' = Map.insert s a m
      putCurrentScope m'
    Just _ ->
      throwError ("scope error: " ++ show s ++ " already defined")

lookup :: MonadError String m => String -> ScopeT v m v
lookup s = do
  m <- getCurrentScope
  case Map.lookup s m of
    Just a ->
      return a
    Nothing -> do
      ms <- getScopeChain
      lookup' s ms

lookup' :: MonadError String m => String -> [Map String v] -> ScopeT v m v
lookup' s xs =
  case xs of
    y:ys ->
      case Map.lookup s y of
        Just a -> 
          return a
        Nothing ->
          lookup' s ys
    [] -> throwError ("scope error: " ++ show s ++ " not defined")

withScope :: Monad m => ScopeT v m a -> ScopeT v m a
withScope (ScopeT m) = ScopeT $ do
  S {..} <- get
  let s = S { currentScope = Map.empty
            , scopeChain = currentScope:scopeChain
            }
  a <- lift $ evalStateT m s
  return a
  