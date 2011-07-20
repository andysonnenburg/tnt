{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, RecordWildCards, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-cse #-}
module Main (main) where

import Control.Applicative
import Control.Monad ((>=>), forM_)
import Control.Monad.Writer

import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL

import Language.TNT.Compiler
import Language.TNT.Location
import Language.TNT.Name
import Language.TNT.Stmt

import System.Console.CmdArgs
import System.FilePath
import System.IO

data TNT = TNT { files :: [FilePath] } deriving (Show, Data, Typeable)

deriving instance Show (Def Located Name)
deriving instance Show (Stmt Located Name)
deriving instance Show (Expr Located Name)

tnt :: Mode (CmdArgs TNT)
tnt = cmdArgsMode TNT { files = def &= args &= typFile }

main :: IO ()
main = do
  TNT {..} <- cmdArgsRun tnt
  forM_ files $ \inputFile -> do
    -- let outputFile = replaceExtension inputFile ".class"
    let className = dropExtension inputFile
    withFile inputFile ReadMode $
      hGetContents >=>
      either
      (hPutStrLn stderr . show)
      print .
      -- (withFile outputFile WriteMode . flip hPutStrLn) .
      compile className
  where
    hPutBinary h s = hSetBinaryMode h True *>
                     BL.hPutStr h s <*
                     hSetBinaryMode h False