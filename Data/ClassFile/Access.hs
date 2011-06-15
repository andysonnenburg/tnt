module Data.ClassFile.Access
       ( ClassAccess
       , FieldAccess
       , MethodAccess
       , FlagSet
       , fromList
       , public
       , private
       , protected
       , static
       , final
       , super
       , synchronized
       , volatile
       , bridge
       , transient
       , varargs
       , native
       , interface
       , abstract
       , strict
       , synthetic
       , annotation
       , enum
       , fromFlags
       ) where

import Data.Bits
import Data.List
import Data.Word

import Prelude hiding (Enum)

class Public a where
  public :: a

class Private a where
  private :: a

class Protected a where
  protected :: a

class Static a where
  static :: a

class Final a where
  final :: a

class Super a where
  super :: a

class Synchronized a where
  synchronized :: a

class Volatile a where
  volatile :: a

class Bridge a where
  bridge :: a

class Transient a where
  transient :: a

class Varargs a where
  varargs :: a

class Native a where
  native :: a

class Interface a where
  interface :: a

class Abstract a where
  abstract :: a

class Strict a where
  strict :: a

class Synthetic a where
  synthetic :: a

class Annotation a where
  annotation :: a

class Enum a where
  enum :: a

class Flag a where
  toFlag :: Word16 -> a
  fromFlag :: a -> Word16

newtype ClassAccess = ClassAccess { unClassAccess :: Word16 }

instance Flag ClassAccess where
  toFlag = ClassAccess
  fromFlag = unClassAccess

newtype FieldAccess = FieldAccess { unFieldAccess :: Word16 }

instance Flag FieldAccess where
  toFlag = FieldAccess
  fromFlag = unFieldAccess

newtype MethodAccess = MethodAccess { unMethodAccess :: Word16 }

instance Flag MethodAccess where
  toFlag = MethodAccess
  fromFlag = unMethodAccess

instance Public ClassAccess where
  public = ClassAccess 0x0001

instance Final ClassAccess where
  final = ClassAccess 0x0010

instance Super ClassAccess where
  super = ClassAccess 0x0020

instance Interface ClassAccess where
  interface = ClassAccess 0x0200

instance Abstract ClassAccess where
  abstract = ClassAccess 0x0400

instance Synthetic ClassAccess where
  synthetic = ClassAccess 0x1000

instance Annotation ClassAccess where
  annotation = ClassAccess 0x2000

instance Enum ClassAccess where
  enum = ClassAccess 0x4000

instance Public FieldAccess where
  public = FieldAccess 0x0001

instance Private FieldAccess where
  private = FieldAccess 0x0002

instance Protected FieldAccess where
  protected = FieldAccess 0x0004

instance Static FieldAccess where
  static = FieldAccess 0x0008

instance Final FieldAccess where
  final = FieldAccess 0x0010

instance Volatile FieldAccess where
  volatile = FieldAccess 0x0040

instance Transient FieldAccess where
  transient = FieldAccess 0x0080

instance Public MethodAccess where
  public = MethodAccess 0x0001

instance Private MethodAccess where
  private = MethodAccess 0x0002

instance Protected MethodAccess where
  protected = MethodAccess 0x0004

instance Static MethodAccess where
  static = MethodAccess 0x0008

instance Final MethodAccess where
  final = MethodAccess 0x0010

instance Synchronized MethodAccess where
  synchronized = MethodAccess 0x0020

instance Native MethodAccess where
  native = MethodAccess 0x0100

instance Abstract MethodAccess where
  abstract = MethodAccess 0x0400

instance Strict MethodAccess where
  strict = MethodAccess 0x0800

newtype FlagSet a = FlagSet { unFlagSet :: Word16 }

fromFlags :: FlagSet a -> Word16
fromFlags = unFlagSet

fromList :: Flag a => [a] -> FlagSet a
fromList = FlagSet . foldl' ((. fromFlag) . (.|.)) 0