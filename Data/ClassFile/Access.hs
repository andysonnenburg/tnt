module Data.ClassFile.Access
       ( public
       , private
       , protected
       , static
       , final
       , volatile
       , transient
       ) where

import Data.Word

newtype AccessFlags = AccessFlags { unAccessFlags :: Word16}

public :: Word16
public = 0x0001

private :: Word16
private = 0x0002

protected :: Word16
protected = 0x0004

static :: Word16
static = 0x0008

final :: Word16
final = 0x0010

volatile :: Word16
volatile = 0x0040

transient :: Word16
transient = 0x0080