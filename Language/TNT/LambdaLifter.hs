{-# LANGUAGE RecordWildCards #-}
module Language.TNT.LambdaLifter (lambdaLift) where

import Control.Comonad
import Control.Monad.State

import Data.Generics
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set, (\\))
import qualified Data.Set as Set

import Language.TNT.Location
import Language.TNT.Name
import Language.TNT.Stmt

data R = R
         { callSet :: Map Name [Name]
         , boundVars :: Map Name [Name]
         }

lambdaLift :: Dec Located Name -> (Dec Located Name, [Dec Located Name])
lambdaLift dec = (dec, [])

-- stronglyConnFuns :: Dec Located Name -> [[Name]]
-- stronglyConnFuns = everywhere (++) (const [] `extQ` funD `extQ` funDefS)
--   where
--     funD :: Dec Located Name -> (Name, Name, [Name])

-- getCallSet :: Name -> LambdaLifter [Name]
-- getCallSet a = do 
--   R {..} <- ask
--   return (callSet ! a)

-- callSet :: Stmt Located Name -> [Name]
-- callSet = q
--   where
--     q = everythingBut (++) $ const ([], False) `extQ` funAppE `extQ` funDefS
    
--     funAppE :: Exp Located Name -> ([Name], Bool)
--     funAppE (FunAppE x _) = ([extract x], False)
--     funAppE _ = ([], False)
    
--     funDefS :: Stmt Located Name -> ([Name], Bool)
--     funDefS (FunDefS _ _ _) = ([], True)
--     funDefS _ = ([], False)

-- getFreeVars :: Data a => Name -> [Name] -> a -> LambdaLifter [Name]
-- getFreeVars a b c = do
--   s@S {..} <- get
--   case Map.lookup a freeVars of
--     Just x ->
--       return x
--     Nothing -> do
--       let defVars = Set.fromList (defVarQ c)
--       let vars = Set.fromList (varQ c)
--       let v = Set.toList (vars \\ defVars)
--       put s { freeVars = Map.insert a v freeVars }
--       return v
--   where
--     defVarQ = everythingBut (++) $ (b, False) `mkQ` defS
    
--     defS :: Stmt Located Name -> ([Name], Bool)
--     defS (ImportS _ x) = ([extract x], False)
--     defS (DefS x _) = ([extract x], False)
--     defS (FunDefS _ _ _) = ([], True)
--     defS _ = ([], False)
    
--     varQ = everythingBut (++) $ const ([], False) `extQ` varE `extQ` funDefS
    
--     varE :: Exp Located Name -> ([Name], Bool)
--     varE (VarE x) = ([x], False)
--     varE (AssignE x _) = ([extract x], False)
--     varE _ = ([], False)
    
--     funDefS :: Stmt Located Name -> ([Name], Bool)
--     funDefS (FunDefS _ _ _) = ([], True)
--     funDefS _ = ([], False)

-- nestedFuns :: Data a => a -> [Name]
-- nestedFuns = everythingBut (++) (([], False) `mkQ` f)
--   where
--     f :: Stmt Located Name -> ([Name], Bool)
--     f (FunDefS a _ _) = ([extract a], True)
--     f _ = ([], True)