module Language.TNT.MemoCombinators
       ( name
       ) where

import Data.Bits
import Data.Function
import Data.MemoCombinators

import Language.TNT.Name
import Language.TNT.Unique

newtype WrappedName = WrapName
                      { unwrapName :: Name
                      } deriving (Show, Eq, Ord)

instance Num WrappedName

instance Bits WrappedName where
  
  complement (WrapName (Name a b)) =
    WrapName (Name (complement (uniqueToWord a)) b)
  shift (WrapName (Name a b)) x = WrapName (Name (shift (uniqueToWord a) x) b)
  rotate (WrapName (Name a b)) x = WrapName (Name (rotate (uniqueToWord a) x) b)
  
  setBit (WrapName (Name a b)) x = WrapName (Name (setBit (uniqueToWord a) x) b)
  clearBit (WrapName (Name a b)) x =
    WrapName (Name (clearBit (uniqueToWord a) x) b)
  testBit (WrapName (Name a _)) x = testBit (uniqueToWord a) x
  bitSize (WrapName (Name a _)) = bitSize (uniqueToWord a)
  isSigned (WrapName (Name a _)) = isSigned (uniqueToWord a)
  shiftL (WrapName (Name a b)) x =
    WrapName (Name (shiftL (uniqueToWord a) x) b)
  shiftR (WrapName (Name a b)) x = WrapName (Name (shiftR (uniqueToWord a) x) b)
  rotateL (WrapName (Name a b)) x =
    WrapName (Name (rotateL (uniqueToWord a) x) b)
  rotateR (WrapName (Name a b)) x =
    WrapName (Name (rotateR (uniqueToWord a) x) b)

name :: Memo Name
name = wrap unwrapName WrapName bits