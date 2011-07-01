module Language.TNT.Stmt
       ( Stmt (..)
       , Expr (..)
       , Property
       ) where

data Stmt w a = Import (w String) (w a)
              | Decl (w a) (w (Expr w a))
              | IfThen
                (w (Expr w a))
                (w [w (Stmt w a)])
              | IfThenElse
                (w (Expr w a))
                (w [w (Stmt w a)])
                (w [w (Stmt w a)])
              | For
                (w (Stmt w a))
                (w (Expr w a))
                (w (Expr w a))
                (w [w (Stmt w a)])
              | ForEach
                (w a)
                (w (Expr w a))
                (w [w (Stmt w a)])
              | While
                (w (Expr w a))
                (w [w (Stmt w a)])
              | Return (w (Expr w a))
              | Throw (w (Expr w a))
              | Expr (Expr w a)
                 
type Property w a = (w String, w (Expr w a))

data Expr w a = Var a
              | Fun
                (w [w a])
                (w [w (Stmt w a)])
              | Number Double
              | String String
              | Char Char
              | Object [w (Property w a)]
              | List [w (Expr w a)]
              | Null
              | Access (w (Expr w a)) (w String)
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