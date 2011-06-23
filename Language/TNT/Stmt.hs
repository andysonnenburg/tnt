module Language.TNT.Stmt
       ( Stmt (..)
       , Expr (..)
       ) where

data Stmt a = Import String a
            | Decl a (Expr a)
            | IfThen (Expr a) [Stmt a]
            | IfThenElse (Expr a) [Stmt a] [Stmt a]
            | For (Stmt a) (Expr a) (Expr a) [Stmt a]
            | ForEach a (Expr a) [Stmt a]
            | Return (Expr a)
            | Expr (Expr a) deriving Show

data Expr a = Var a
            | Fun [a] [Stmt a]
            | Numeric Double
            | String String
            | List [Expr a]
            | Access (Expr a) String
            | Mutate (Expr a) String (Expr a)
            | Assign a (Expr a)
            | App (Expr a) [Expr a] deriving Show