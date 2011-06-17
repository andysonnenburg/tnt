{-# LANGUAGE GADTs #-}
module Test where

import Data.List.Parameterized

import Type.List hiding (List)

data Test i j where
  Test1 :: Test xs (Cons Int xs)
  Test2 :: Test (Cons Int xs) xs
  
test = Test1 :< Test2 :< Empty