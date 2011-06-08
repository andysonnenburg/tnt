module Data.ClassFile.Class (Class (..)) where

import Data.ClassFile.Access

newtype C a = C { unC :: VersionT ConstantPool a }

type FieldInfoC = C FieldInfo
type MethodInfoC = C MethodInfo
type AttributeInfoC = C AttributeInfo

runC :: Word16 ->
        Word16 ->
        AccessSet ->
        String ->
        Maybe String ->
        [String] ->
        [FieldInfoC] ->
        [MethodInfoC] ->
        [AttributeInfoC] ->
        ClassFile
runC minorVersion majorVersion access thisClass superClass interfaces fields methods attributes = a
  where
    (a, constantPoolLength, constantPool) =
      runConstantPool . flip runVersionT minorVersion majorVersion $ do
        thisClass <- lookupClass thisClass
        superClass <- maybe (return 0) lookupClass superClass
        interfaces <- mapM lookupClass interfaces
        fields <- sequence fields
        methods <- sequence methods
        attributes <- sequence attributes
        return ClassFile { minorVersion
                         , majorVersion
                         , constantPoolLength
                         , constantPool
                         , thisClass
                         , superClass
                         , interfaces
                         , fields
                         , methods
                         , attributes
                         }

fieldC :: FieldType a => String -> a -> FieldInfoC
  