-- |

module Tokens
  (
    TokenType
  )
where

data TokenType = LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  | Comma
  | Dot
  | Minus
  | Plus
  | SemiColon
  | Slash
  | Star
  | Bang
  | BangEq
  | Equal
  | EqualEqual
  | Greater
  | GreaterEq
  | Less
  | LessEq
  | Identifier
  | String
  | Number
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  | Eof
  deriving (Enum)

data Token = Token
  {
    tokenType :: TokenType,
    lexeme :: String,
    literal :: String,
    line :: Int
  }
