module Data.ClassFile.Access
       ( Access (..)
       , AccessSet
       , fromList
       , fromFlags
       , toFlags
       , public
       , private
       , protected
       , static
       , final
       , volatile
       , transient
       ) where

import Data.Bits
import Data.List
import Data.Word

data Access = Public
            | Private
            | Protected
            | Static
            | Final
            | Volatile
            | Transient deriving Enum

newtype AccessSet = AccessSet { unAccessSet :: Word16}

toFlag :: Access -> Word16
toFlag = shiftL 1 . fromEnum

fromList :: [Access] -> AccessSet
fromList = AccessSet . foldl' ((. toFlag) . (.|.)) 0

fromFlags :: Word16 -> AccessSet
fromFlags = AccessSet

toFlags :: AccessSet -> Word16
toFlags = unAccessSet

public :: Access
public = Public

private :: Access
private = Private

protected :: Access
protected = Protected

static :: Access
static = Static

final :: Access
final = Final

volatile :: Access
volatile = Volatile

transient :: Access
transient = Transient