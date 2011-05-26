module Control.Monad.Code.Opcode where

import Data.Word

nop :: Word8
nop = 0x00

aconst_null :: Word8
aconst_null = 0x01

iconst_m1 :: Word8
iconst_m1 = 0x02

iconst_0 :: Word8
iconst_0 = 0x03

iconst_1 :: Word8
iconst_1 = 0x04

iconst_2 :: Word8
iconst_2 = 0x05

iconst_3 :: Word8
iconst_3 = 0x06

iconst_4 :: Word8
iconst_4 = 0x07

iconst_5 :: Word8
iconst_5 = 0x08

lconst_0 :: Word8
lconst_0 = 0x09

lconst_1 :: Word8
lconst_1 = 0x0a

fconst_0 :: Word8
fconst_0 = 0x0b

fconst_1 :: Word8
fconst_1 = 0x0c

fconst_2 :: Word8
fconst_2 = 0x0d

dconst_0 :: Word8
dconst_0 = 0x0e

dconst_1 :: Word8
dconst_1 = 0x0f

bipush :: Word8
bipush = 0x10

sipush :: Word8
sipush = 0x11

ldc :: Word8
ldc = 0x12

ldc_w :: Word8
ldc_w = 0x13

ldc2_w :: Word8
ldc2_w = 0x14

iload :: Word8
iload = 0x15

lload :: Word8
lload = 0x16

fload :: Word8
fload = 0x17

dload :: Word8
dload = 0x18

aload :: Word8
aload = 0x19

iload_0 :: Word8
iload_0 = 0x1a

iload_1 :: Word8
iload_1 = 0x1b

iload_2 :: Word8
iload_2 = 0x1c

iload_3 :: Word8
iload_3 = 0x1d

lload_0 :: Word8
lload_0 = 0x1e

lload_1 :: Word8
lload_1 = 0x1f

lload_2 :: Word8
lload_2 = 0x20

lload_3 :: Word8
lload_3 = 0x21

fload_0 :: Word8
fload_0 = 0x22

fload_1 :: Word8
fload_1 = 0x23

fload_2 :: Word8
fload_2 = 0x24

fload_3 :: Word8
fload_3 = 0x25

dload_0 :: Word8
dload_0 = 0x26

dload_1 :: Word8
dload_1 = 0x27

dload_2 :: Word8
dload_2 = 0x28

dload_3 :: Word8
dload_3 = 0x29

aload_0 :: Word8
aload_0 = 0x2a

aload_1 :: Word8
aload_1 = 0x2b

aload_2 :: Word8
aload_2 = 0x2c

aload_3 :: Word8
aload_3 = 0x2d

iaload :: Word8
iaload = 0x2e

laload :: Word8
laload = 0x2f

faload :: Word8
faload = 0x30

daload :: Word8
daload = 0x31

aaload :: Word8
aaload = 0x32

baload :: Word8
baload = 0x33

caload :: Word8
caload = 0x34

saload :: Word8
saload = 0x35

istore :: Word8
istore = 0x36

lstore :: Word8
lstore = 0x37

fstore :: Word8
fstore = 0x38

dstore :: Word8
dstore = 0x39

astore :: Word8
astore = 0x3a

istore_0 :: Word8
istore_0 = 0x3b

istore_1 :: Word8
istore_1 = 0x3c

istore_2 :: Word8
istore_2 = 0x3d

istore_3 :: Word8
istore_3 = 0x3e

lstore_0 :: Word8
lstore_0 = 0x3f

lstore_1 :: Word8
lstore_1 = 0x40

lstore_2 :: Word8
lstore_2 = 0x41

lstore_3 :: Word8
lstore_3 = 0x42

fstore_0 :: Word8
fstore_0 = 0x43

fstore_1 :: Word8
fstore_1 = 0x44

fstore_2 :: Word8
fstore_2 = 0x45

fstore_3 :: Word8
fstore_3 = 0x46

dstore_0 :: Word8
dstore_0 = 0x47

dstore_1 :: Word8
dstore_1 = 0x48

dstore_2 :: Word8
dstore_2 = 0x49

dstore_3 :: Word8
dstore_3 = 0x4a

astore_0 :: Word8
astore_0 = 0x4b

astore_1 :: Word8
astore_1 = 0x4c

astore_2 :: Word8
astore_2 = 0x4d

astore_3 :: Word8
astore_3 = 0x4e

iastore :: Word8
iastore = 0x4f

lastore :: Word8
lastore = 0x50

fastore :: Word8
fastore = 0x51

dastore :: Word8
dastore = 0x52

aastore :: Word8
aastore = 0x53

bastore :: Word8
bastore = 0x54

castore :: Word8
castore = 0x55

sastore :: Word8
sastore = 0x56

pop :: Word8
pop = 0x57

pop2 :: Word8
pop2 = 0x58

dup :: Word8
dup = 0x59

dup_x1 :: Word8
dup_x1 = 0x5a

dup_x2 :: Word8
dup_x2 = 0x5b

dup2 :: Word8
dup2 = 0x5c

dup2_x1 :: Word8
dup2_x1 = 0x5d

dup2_x2 :: Word8
dup2_x2 = 0x5e

swap :: Word8
swap = 0x5f

iadd :: Word8
iadd = 0x60

ladd :: Word8
ladd = 0x61

fadd :: Word8
fadd = 0x62

dadd :: Word8
dadd = 0x63

isub :: Word8
isub = 0x64

lsub :: Word8
lsub = 0x65

fsub :: Word8
fsub = 0x66

dsub :: Word8
dsub = 0x67

imul :: Word8
imul = 0x68

lmul :: Word8
lmul = 0x69

fmul :: Word8
fmul = 0x6a

dmul :: Word8
dmul = 0x6b

idiv :: Word8
idiv = 0x6c

ldiv :: Word8
ldiv = 0x6d

fdiv :: Word8
fdiv = 0x6e

ddiv :: Word8
ddiv = 0x6f

irem :: Word8
irem = 0x70

lrem :: Word8
lrem = 0x71

frem :: Word8
frem = 0x72

drem :: Word8
drem = 0x73

ineg :: Word8
ineg = 0x74

lneg :: Word8
lneg = 0x75

fneg :: Word8
fneg = 0x76

dneg :: Word8
dneg = 0x77

ishl :: Word8
ishl = 0x78

lshl :: Word8
lshl = 0x79

ishr :: Word8
ishr = 0x7a

lshr :: Word8
lshr = 0x7b

iushr :: Word8
iushr = 0x7c

lushr :: Word8
lushr = 0x7d

iand :: Word8
iand = 0x7e

land :: Word8
land = 0x7f

ior :: Word8
ior = 0x80

lor :: Word8
lor = 0x81

ixor :: Word8
ixor = 0x82

lxor :: Word8
lxor = 0x83

iinc :: Word8
iinc = 0x84

i2l :: Word8
i2l = 0x85

i2f :: Word8
i2f = 0x86

i2d :: Word8
i2d = 0x87

l2i :: Word8
l2i = 0x88

l2f :: Word8
l2f = 0x89

l2d :: Word8
l2d = 0x8a

f2i :: Word8
f2i = 0x8b

f2l :: Word8
f2l = 0x8c

f2d :: Word8
f2d = 0x8d

d2i :: Word8
d2i = 0x8e

d2l :: Word8
d2l = 0x8f

d2f :: Word8
d2f = 0x90

i2b :: Word8
i2b = 0x91

i2c :: Word8
i2c = 0x92

i2s :: Word8
i2s = 0x93

lcmp :: Word8
lcmp = 0x94

fcmpl :: Word8
fcmpl = 0x95

fcmpg :: Word8
fcmpg = 0x96

dcmpl :: Word8
dcmpl = 0x97

dcmpg :: Word8
dcmpg = 0x98

ifeq :: Word8
ifeq = 0x99

ifne :: Word8
ifne = 0x9a

iflt :: Word8
iflt = 0x9b

ifge :: Word8
ifge = 0x9c

ifgt :: Word8
ifgt = 0x9d

ifle :: Word8
ifle = 0x9e

if_icmpeq :: Word8
if_icmpeq = 0x9f

if_icmpne :: Word8
if_icmpne = 0xa0

if_icmplt :: Word8
if_icmplt = 0xa1

if_icmpge :: Word8
if_icmpge = 0xa2

if_icmpgt :: Word8
if_icmpgt = 0xa3

if_icmple :: Word8
if_icmple = 0xa4

if_acmpeq :: Word8
if_acmpeq = 0xa5

if_acmpne :: Word8
if_acmpne = 0xa6

goto :: Word8
goto = 0xa7

jsr :: Word8
jsr = 0xa8

ret :: Word8
ret = 0xa9

tableswitch :: Word8
tableswitch = 0xaa

lookupswitch :: Word8
lookupswitch = 0xab

ireturn :: Word8
ireturn = 0xac

lreturn :: Word8
lreturn = 0xad

freturn :: Word8
freturn = 0xae

dreturn :: Word8
dreturn = 0xaf

areturn :: Word8
areturn = 0xb0

return :: Word8
return = 0xb1

getstatic :: Word8
getstatic = 0xb2

putstatic :: Word8
putstatic = 0xb3

getfield :: Word8
getfield = 0xb4

putfield :: Word8
putfield = 0xb5

invokevirtual :: Word8
invokevirtual = 0xb6

invokespecial :: Word8
invokespecial = 0xb7

invokestatic :: Word8
invokestatic = 0xb8

invokeinterface :: Word8
invokeinterface = 0xb9

xxxunusedxxx1 :: Word8
xxxunusedxxx1 = 0xba

new :: Word8
new = 0xbb

newarray :: Word8
newarray = 0xbc

anewarray :: Word8
anewarray = 0xbd

arraylength :: Word8
arraylength = 0xbe

athrow :: Word8
athrow = 0xbf

checkcast :: Word8
checkcast = 0xc0

instanceof :: Word8
instanceof = 0xc1

monitorenter :: Word8
monitorenter = 0xc2

monitorexit :: Word8
monitorexit = 0xc3

wide :: Word8
wide = 0xc4

multianewarray :: Word8
multianewarray = 0xc5

ifnull :: Word8
ifnull = 0xc6

ifnonnull :: Word8
ifnonnull = 0xc7

goto_w :: Word8
goto_w = 0xc8

jsr_w :: Word8
jsr_w = 0xc9

breakpoint :: Word8
breakpoint = 0xca

impdep1 :: Word8
impdep1 = 0xfe

impdep2 :: Word8
impdep2 = 0xff