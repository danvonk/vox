-- |

module Parser where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Language
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token

import Lexer
import AST


binary  name label = Infix (do
                               reservedOp name
                               return label
                           )
prefix  name label = Prefix (do{ reservedOp name; return label })
postfix name label = Postfix (do{ reservedOp name; return label })

opTable =
        [[prefix "!" Not],
         [binary "*" Mul AssocLeft,
          binary "/" Div AssocLeft],
         [binary "+" Add AssocLeft,
          binary "-" Sub AssocLeft],
         [binary "==" Equal AssocLeft,
          binary "<" Less AssocLeft,
          binary "<=" LessEq AssocLeft,
          binary ">" Great AssocLeft,
          binary ">=" GreatEq AssocLeft],
         [binary "&&" And AssocLeft,
          binary "||" Or AssocLeft]
        ]

opExpr :: Parser Expr
opExpr = buildExpressionParser opTable term


var :: Parser Expr
var = CVar <$> many1 alphaNum

nil' :: Parser Expr
nil' = Nil <$ string "nil"

num' :: Parser Expr
num' = CInt . read <$> many1 digit

bool' :: Parser Expr
bool' = CBool True <$ string "true"
  <|> CBool False <$ string "false"

expr :: Parser Expr
expr = opExpr
  <|> term

term :: Parser Expr
term = num'
  <|> nil'
  <|> bool'
  <|> var
  <|> parens expr


parseString :: String -> Either ParseError Expr
parseString = parse (expr <* eof) ""

parseFile :: FilePath -> IO (Either ParseError Expr)
parseFile = parseFromFile (expr <* eof)
