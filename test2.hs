{-# LANGUAGE TypeFamilies #-}
module Test2 where

import Test1

instance TestF Int Int where
  type Test Int Int = Double