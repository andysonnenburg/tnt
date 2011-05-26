module Data.ClassFile.Attribute (Attribute (..)) where

import Data.ByteString.Lazy (ByteString)

data Attribute = Attribute String ByteString