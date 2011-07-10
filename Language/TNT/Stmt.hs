module Language.TNT.Stmt
       ( Top (..)
       , Stmt (..)
       , Expr (..)
       , Property
       ) where

import Language.TNT.Unique

data Top w a = Top Unique [w (Stmt w a)]

data Stmt w a = Import (w String) (w a)
              | Decl (w a) (w (Expr w a))
              | IfThen
                (w (Expr w a))
                (w (Stmt w a))
              | IfThenElse
                (w (Expr w a))
                (w (Stmt w a))
                (w (Stmt w a))
              | For
                (w (Stmt w a))
                (w (Expr w a))
                (w (Expr w a))
                (w (Stmt w a))
              | ForEach
                (w a)
                (w (Expr w a))
                (w (Stmt w a))
              | While
                (w (Expr w a))
                (w (Stmt w a))
              | Return (w (Expr w a))
              | Throw (w (Expr w a))
              | Expr (w (Expr w a))
              | Block [w (Stmt w a)]
                
type Property w a = (w String, w (Expr w a))

data Expr w a = Var (w a)
              | Fun
                Unique
                (w [w a])
                (w (Stmt w a))
              | Number Double
              | String String
              | Char Char
              | Null
              | Bool Bool
              | Object [w (Property w a)]
              | List [w (Expr w a)]
              | Access
                (w (Expr w a))
                (w String)
              | Mutate
                (w (Expr w a))
                (w String)
                (w (Expr w a))
              | Assign
                (w a)
                (w (Expr w a))
              | App
                (w (Expr w a))
                (w [w (Expr w a)])
              | Or
                (w (Expr w a))
                (w (Expr w a))
              | And
                (w (Expr w a))
                (w (Expr w a))
