module Language.TNT.Resolver (resolve) where

import Language.TNT.Scope

resolve :: [Stmt String] -> Either Message [Stmt Var]
resolve = undefined