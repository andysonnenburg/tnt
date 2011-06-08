{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main (main) where

import Control.Applicative
import Control.Monad ((>=>), forM_)

import qualified Data.ByteString.Lazy as BL

import Language.Brainfuck.Compiler

import System.Console.CmdArgs
import System.FilePath
import System.IO

import Prelude hiding (Monad (..))

data BF = BF { files :: [FilePath] } deriving (Show, Data, Typeable)

bf :: Mode (CmdArgs BF)
bf = cmdArgsMode BF { files = def &= args &= typ "FILES" }

main :: IO ()
main = do
  BF {..} <- cmdArgsRun bf
  forM_ files $ \inputFile -> do
    let outputFile = replaceExtension inputFile ".class"
        className = dropExtension inputFile
    withFile inputFile ReadMode $
      BL.hGetContents >=>
      either
      (hPutStrLn stderr)
      (withFile outputFile WriteMode . flip hPutBinary) .
      compile className
  where
    hPutBinary h s = hSetBinaryMode h True *>
                     BL.hPutStr h s <*
                     hSetBinaryMode h False
    
