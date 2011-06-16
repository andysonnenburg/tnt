{-# LANGUAGE NoImplicitPrelude #-}
module Data.ClassFile.Access
       ( ClassAccess
       , FieldAccess
       , MethodAccess
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
       ) where

import Control.Applicative

import Data.Binary
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Function
import Data.List
import Data.Monoid

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

newtype ClassAccess = ClassAccess { unClassAccess :: Word16 }

instance Monoid ClassAccess where
  mempty = ClassAccess 0
  mappend = (ClassAccess .) . (.|.) `on` unClassAccess
  mconcat = ClassAccess . foldl' ((. unClassAccess) . (.|.)) 0

instance Binary ClassAccess where
  get = ClassAccess <$> getWord16be
  put = putWord16be . unClassAccess

newtype FieldAccess = FieldAccess { unFieldAccess :: Word16 }

instance Monoid FieldAccess where
  mempty = FieldAccess 0
  mappend = (FieldAccess .) . (.|.) `on` unFieldAccess
  mconcat = FieldAccess . foldl' ((. unFieldAccess) . (.|.)) 0

instance Binary FieldAccess where
  get = FieldAccess <$> getWord16be
  put = putWord16be . unFieldAccess

newtype MethodAccess = MethodAccess { unMethodAccess :: Word16 }

instance Monoid MethodAccess where
  mempty = MethodAccess 0
  mappend = (MethodAccess .) . (.|.) `on` unMethodAccess
  mconcat = MethodAccess . foldl' ((. unMethodAccess) . (.|.)) 0 

instance Binary MethodAccess where
  get = MethodAccess <$> getWord16be
  put = putWord16be . unMethodAccess

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