-- |

module AST
  (
    Expr (..),
    Stmt (..)
  ) where

data Expr
  = CInt Integer
  | CBool Bool
  | CVar String
  | Nil
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Equal Expr Expr
  | Less Expr Expr
  | LessEq Expr Expr
  | Great Expr Expr
  | GreatEq Expr Expr
  deriving Show

data Stmt
  = String := Expr
  | Var String Expr
  | If Expr Stmt Stmt
  | While Expr Stmt
  | Print Expr
  | Seq [Stmt]
  deriving Show

-- instance Eq Expr where
--   CInt x == CInt y = x == y
--   CBool a == CBool b = a == b
--   CVar s == CVar t = s == t
--   Nil == Nil = True
