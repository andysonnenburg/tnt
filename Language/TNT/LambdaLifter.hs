module Language.TNT.LambdaLifter (lambdaLift) where

lambdaLift :: Dec Located Name -> (Dec Located Name, [Dec Located Name])
lambdaLift dec = (dec, [])