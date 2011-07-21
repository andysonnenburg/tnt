{-# LANGUAGE PatternGuards #-}
module Language.TNT.FunBoxer (boxFuns) where

import Control.Applicative
import Control.Comonad

import Data.Generics
import qualified Data.Set as Set

import Language.TNT.Location
import Language.TNT.Name
import Language.TNT.Stmt

boxFuns :: Dec Located Name -> Dec Located Name
boxFuns dec = everywhere (mkT unboxFunApp) .
              everywhere (mkT boxFun) $ dec
  where
    boxFun :: Exp Located Name -> Exp Located Name
    boxFun (VarE a)
      | Set.member a funs = FunE a []
    boxFun x = x
    
    unboxFunApp :: Exp Located Name -> Exp Located Name
    unboxFunApp (AppE a b)
      | FunE x _ <- extract a = FunAppE (x <$ a) b
    unboxFunApp x = x
    
    funs = Set.fromList (q dec)
    
    q = everything (++) (const [] `extQ` funD `extQ` funDefS)
    
    funD :: Dec Located Name -> [Name]
    funD (FunD a _ _) = [a]
    
    funDefS :: Stmt Located Name -> [Name]
    funDefS (FunDefS a _ _) = [extract a]
    funDefS _ = []