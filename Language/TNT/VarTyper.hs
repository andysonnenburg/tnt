{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module Language.TNT.VarTyper (typeVars) where

import Control.Applicative
import Control.Comonad
import Control.Monad.State

import Data.Generics
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Maybe

import Language.TNT.Location
import Language.TNT.Name
import Language.TNT.Stmt
import Language.TNT.Var

data S = S
         { enclosingName :: Name
         , names :: Map Name Name
         , varSorts :: Map Name VarSort
         }

typeVars :: Dec Located Name -> Dec Located Var
typeVars (FunD a b c) = FunD (f a) (map f b) (fmap f <$> c)
  where
    f x = Var (fromMaybe Val (Map.lookup x varSorts')) (Map.lookup x names') x
    
    names' = names s
    varSorts' = varSorts s
    
    s = q c
    
    q = flip execState initState . (enclosingQ >=> funQ >=> valQ >=> refQ)
    
    initState = S a Map.empty Map.empty

enclosingQ :: Data a => a -> State S ()
enclosingQ = everything (>>) (return () `mkQ` enclosingS)
    
funQ :: Data a => a -> State S ()
funQ = everything (>>) (return () `mkQ` funS)
    
valQ :: Data a => a -> State S ()
valQ = everything (>>) (const (return ()) `extQ` valS `extQ` valE)
    
refQ :: Data a => a -> State S ()
refQ = everything (>>) (return () `mkQ` refE)
    
enclosingS :: Stmt Located Name -> State S ()
enclosingS (ImportS _ a) = do
  s@S {..} <- get
  put s { names = Map.insert (extract a) enclosingName names }
enclosingS (DefS a _) = do
  s@S {..} <- get
  put s { names = Map.insert (extract a) enclosingName names }
enclosingS (FunDefS a (extract -> b) _) = do
  s@S {..} <- get
  let m = Map.insert (extract a) enclosingName names
  let m' = foldl' (\x y -> Map.insert (extract y) enclosingName x) m b
  put s { names = m' }
enclosingS (ForEachS a _ _) = do
  s@S {..} <- get
  put s { names = Map.insert (extract a) enclosingName names }
enclosingS _ = return ()
    
funS :: Stmt Located Name -> State S ()
funS (FunDefS a _ _) = do
  s@S {..} <- get
  put s { varSorts = Map.insert (extract a) Fun varSorts }
funS _ = return ()

valS :: Stmt Located Name -> State S ()
valS (ImportS _ a) = do
  s@S {..} <- get
  put s { varSorts = Map.insert (extract a) Val varSorts }
valS (DefS a _) = do
  s@S {..} <- get
  put s { varSorts = Map.insert (extract a) Val varSorts }
valS _ = return ()

valE :: Exp Located Name -> State S ()
valE (AssignE a _) = do
  s@S {..} <- get
  put s { varSorts = Map.insert (extract a) Val varSorts }
valE _ = return ()
    
refE :: Exp Located Name -> State S ()
refE (VarE a) = do
  s@S {..} <- get
  when (varSorts ! a == Val && names ! a /= enclosingName) $ do
    put s { varSorts = Map.insert a Ref varSorts }
refE _ = return ()
    