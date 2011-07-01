module Language.TNT.Stmt
       ( Stmt (..)
       , Expr (..)
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
               | Return (w (Expr w a))
               | Expr (Expr w a)

data Expr w a = Var a
               | Fun
                 (w [w a])
                 (w [w (Stmt w a)])
               | Numeric Double
               | String String
               | List [w (Expr w a)]
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