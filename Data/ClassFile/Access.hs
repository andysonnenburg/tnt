module Data.ClassFile.Access
       ( Access
       , AccessSet
       , fromList
       , fromFlags
       , toFlags
       , public
       , private
       , protected
       , static
       , final
       , synchronized
       , volatile
       , bridge
       , transient
       , varargs
       , native
       , abstract
       , strict
       , synthetic
       , enum
       ) where

import Data.Bits
import Data.List
import Data.Word

data Access = Access { unAccess :: Word16 }

newtype AccessSet = AccessSet { unAccessSet :: Word16}

toFlag :: Access -> Word16
toFlag = unAccess

fromList :: [Access] -> AccessSet
fromList = AccessSet . foldl' ((. toFlag) . (.|.)) 0

fromFlags :: Word16 -> AccessSet
fromFlags = AccessSet

toFlags :: AccessSet -> Word16
toFlags = unAccessSet

public :: Access
public = Access 0x0001

private :: Access
private = Access 0x0002

protected :: Access
protected = Access 0x0004

static :: Access
static = Access 0x0008

final :: Access
final = Access 0x0010

synchronized :: Access
synchronized = Access 0x0020

volatile :: Access
volatile = Access 0x0040

bridge :: Access
bridge = Access 0x0040

transient :: Access
transient = Access 0x0080

varargs :: Access
varargs = Access 0x0080

native :: Access
native = Access 0x0100

abstract :: Access
abstract = Access 0x0400

strict :: Access
strict = Access 0x0800

synthetic :: Access
synthetic = Access 0x1000

enum :: Access
enum = Access 0x4000