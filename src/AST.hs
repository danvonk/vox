-- |

module AST
  (
    Expr (..)
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
