{-# LANGUAGE
FlexibleContexts,
FlexibleInstances,
FunctionalDependencies,
MultiParamTypeClasses,
TypeFamilies #-}
module Main (main) where

main :: IO ()
main = f (M :: M B) `seq` putStrLn "what???"

data M a = M
data A
data B


class PopF a b where
  type Pop a b

instance PopF A (A, a) where
  type Pop A (A, a) = a

f :: PopF A a => M a -> M (Pop A a)
f M = M

{-
class Pop a b c | a b -> c, b c -> a, c a -> b
instance Pop A (A, a) a

f :: Pop A a a' => M a -> M a'
f M = M
-}