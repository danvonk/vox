-- |

module AST
  (
    Expr (..),
    Stmt (..),
    UnaryOp (..),
    BinaryOp (..)
  ) where

data Expr
  = CInt Integer
  | CBool Bool
  | CVar String
  | Nil
  | Variable String
  | UnOp UnaryOp Expr
  | BinOp BinaryOp Expr Expr
  | FunCall String [Expr]
  deriving Show

data UnaryOp
   = Not
   deriving Show

data BinaryOp
  = Add
  | Sub
  | Mul
  | Div
  | And
  | Or
  | Equal
  | Neq
  | Less
  | LessEq
  | Great
  | GreatEq
  deriving Show

data Stmt
  = Var String Expr
  | If Expr Stmt Stmt
  | While Expr Stmt
  | Print Expr
  | Return (Maybe Expr)
  | FunStmt String [String] [Stmt]
  | Seq [Stmt]
  deriving Show

-- instance Eq Expr where
--   CInt x == CInt y = x == y
--   CBool a == CBool b = a == b
--   CVar s == CVar t = s == t
--   Nil == Nil = True
