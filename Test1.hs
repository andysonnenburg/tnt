{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module Test1 (Test) where

class TestF a b where
  type Test a b

