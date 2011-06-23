{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main (main) where

import Control.Applicative
import Control.Monad ((>=>), forM_)
import Control.Monad.Writer

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

import Language.TNT.Compiler

import System.Console.CmdArgs
import System.FilePath
import System.IO

data TNT = TNT { files :: [FilePath] } deriving (Show, Data, Typeable)

tnt :: Mode (CmdArgs TNT)
tnt = cmdArgsMode TNT { files = def &= args &= typFile }

main :: IO ()
main = do
  TNT {..} <- cmdArgsRun tnt
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