{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main (main) where

import Control.Monad ((>=>), forM_)
import Control.Monad.Writer

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

import Language.TNT.Lexer
import Language.TNT.Parser
import Language.TNT.Token

import System.Console.CmdArgs
import System.IO

data TNT = TNT { files :: [FilePath] } deriving (Show, Data, Typeable)

tnt :: Mode (CmdArgs TNT)
tnt = cmdArgsMode TNT { files = def &= args &= typFile }

main :: IO ()
main = do
  TNT {..} <- cmdArgsRun tnt
  forM_ files $ \inputFile -> do
    withFile inputFile ReadMode $
      BL.hGetContents >=>
      either (hPutStrLn stderr) print .
      parse
  where
    f = do
      x <- alexMonadScan
      case x of
        EOF -> return []
        _ -> do
          xs <- f
          return (x:xs)