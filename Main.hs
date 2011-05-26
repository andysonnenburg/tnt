module Main (main) where

import Control.Monad.Code
import Control.Monad.ConstantPool
import Control.Monad.Fix
import qualified Control.Monad.Parameterized as Parameterized

import qualified Data.ByteString.Lazy as BL
import Data.ClassFile.Attribute

import Language.Brainfuck.Emitter
import Language.Brainfuck.Parser

import System.IO

main :: IO ()

main = BL.getContents >>=
       either (hPutStrLn stderr) (print .
                                  f .
                                  runConstantPool .
                                  execCode .
                                  emit) . parse
  where
    f (Attribute _ x, n, xs) = (BL.unpack x, n, xs)

-- main =
--   print .
--   f .
--   runConstantPool .
--   execCode .
--   mfix $ \end -> do
--     begin <- goto end
--     goto begin
--     end <- nop
--     Parameterized.return end
--  where
--    f (Attribute _ x, n, xs) = (BL.unpack x, n, xs)

