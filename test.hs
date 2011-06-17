{-# LANGUAGE DoRec, FlexibleContexts, RecordWildCards #-}
module Main (main) where

import Control.Applicative hiding (Const (..))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer

import Data.Array.Unboxed
import Data.Int
import Data.List
import Data.Ord
import Data.Word

data Expr = Const Int32
          | Var Word16
          | Add Expr Expr
          | Subtract Expr Expr deriving (Show, Eq)

instance Num Expr where
  (+) = Add
  (*) = undefined
  (-) = Subtract
  fromInteger = Const . fromIntegral
  abs = undefined
  signum = undefined

type Eval = WriterT [String] (ReaderT (UArray Word16 Int32) [])

data S = S { varCount :: Word16
           , varDomain :: [[Int32]]
           , codeLength :: Expr
           , code :: Eval ()
           }

type Code = State S

newVar :: [Int32] -> Code Expr
newVar xs = do
  s@S {..} <- get
  put $ s { varCount = varCount + 1
          , varDomain = xs:varDomain
          }
  return . Var $ varCount

a :: Word16 -> Code Expr
a x = do
  s@S {..} <- get
  put $ s { codeLength = codeLength + fromIntegral x
          , code = code >> do
            source <- eval codeLength
            tell [show source ++ ": a " ++ show x]
          }
  return codeLength

eval e = case e of
  Const x -> return x
  Var x -> (! x) <$> ask
  Add x y -> (+) <$> eval x <*> eval y
  Subtract x y -> (-) <$> eval x <*> eval y

switch :: Code Expr
switch = do
  y <- newVar [0 .. 3]
  s@S {..} <- get
  put $ s { codeLength = codeLength + y
          , code = do code
                      size <- eval y
                      index <- eval codeLength
                      guard ((index + size) `mod` 4 == 0)
                      tell [show index ++ ": switch " ++ show size]
          }
  return codeLength

goto :: Expr -> Code Expr
goto x = do
  y <- newVar [3, 5]
  s@S {..} <- get
  put $ s { codeLength = codeLength + y
          , code = code >> do
            size <- eval y
            source <- eval codeLength
            target <- eval x
            let offset = target - source
            guard (size == 5 ||
                   offset >= fromIntegral (minBound :: Int16) &&
                   offset <= fromIntegral (maxBound :: Int16))
            tell [show source ++ ": " ++ 
                  (if size == 3 then "goto" else "goto_w") ++
                  " " ++ show offset]
          }
  return codeLength

runCode :: Code a -> [String]
runCode m =
  let S {..} = execState m (S 0 [] 0 (return ()))
  in snd . head . sortBy (comparing fst) $ do
    let varDomain' = reverse varDomain
        code' = code >> eval codeLength
    env <- listArray (0, varCount - 1) <$> f varDomain'
    (runReaderT . runWriterT) code' env
  where
    f [] = [[]]
    f (x:xs) = [y:ys | y <- x, ys <- f xs]
  
main :: IO ()
main = putStr . unlines . runCode $ do
  rec
    x <- a 16384
    goto y
    a 16380
    goto x
    switch
    a 16380
    y <- a 16380
  return ()