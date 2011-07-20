{-# LANGUAGE PatternGuards #-}
module Language.TNT.ClosureLifter (closureLift) where

import Control.Comonad

import Data.Generics
import qualified Data.Set as Set

import Language.TNT.Location
import Language.TNT.Name
import Language.TNT.Stmt

closureLift :: Dec Located Name -> Dec Located Name
closureLift dec = everywhere' (mkT transform) dec
  where
    transform :: Exp Located Name -> Exp Located Name
    transform (AppE a b)
      | VarE a' <- extract a,
        Set.member (extract a') funs = FunAppE a' b
    transform (VarE a)
      | Set.member (extract a) funs = FunE a
    transform x = x
    
    funs = Set.fromList (everything (++) (const [] `extQ` d `extQ` s) dec)
    
    d :: Dec Located Name -> [Name]
    d (FunD a _ _) = [a]
    
    s :: Stmt Located Name -> [Name]
    s (FunDefS a _ _) = [extract a]
    s _ = []