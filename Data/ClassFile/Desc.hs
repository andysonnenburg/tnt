module Data.ClassFile.Desc
       ( Desc (..)
       , methodDesc
       ) where

import Data.Word

class Desc a where
  descs :: a -> ShowS
  desc :: a -> String
  stackSize :: a -> Word16
  
  desc = flip descs ""
  descs = (++) . desc
  stackSize _ = 1

methodDesc :: (Desc a, Desc b) => a -> b -> String
methodDesc a b = (showChar '(' . descs a . showChar ')' . descs b) ""