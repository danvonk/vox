-- |

module Parser
  (
    parseFile,
    parseString
  ) where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr

import Lexer
import AST

binary  name label = Infix (do
                               reservedOp name
                               return label
                           )
prefix  name label = Prefix (do{ reservedOp name; return label })
postfix name label = Postfix (do{ reservedOp name; return label })

opTable =
        [
          [prefix "!" (UnOp Not)],
         [binary "*"  (BinOp Mul) AssocLeft,
          binary "/" (BinOp Div) AssocLeft],
         [binary "+" (BinOp Add) AssocLeft,
          binary "-" (BinOp Sub) AssocLeft],
         [binary "==" (BinOp Equal) AssocLeft,
          binary "<" (BinOp Less) AssocLeft,
          binary "<=" (BinOp LessEq) AssocLeft,
          binary ">" (BinOp Great) AssocLeft,
          binary ">=" (BinOp GreatEq) AssocLeft],
         [binary "&&" (BinOp And) AssocLeft,
          binary "||" (BinOp Or) AssocLeft]
        ]

opExpr :: Parser Expr
opExpr = buildExpressionParser opTable term

var :: Parser Expr
var = CVar <$> identifier

nil :: Parser Expr
nil = Nil <$ reserved "nil"

num :: Parser Expr
num = CInt <$> integer

bool :: Parser Expr
bool = CBool True <$ reserved "true"
  <|> CBool False <$ reserved "false"


expr :: Parser Expr
expr = opExpr
  <|> term

term :: Parser Expr
term = num
  <|> nil
  <|> bool
  <|> var
  <|> parens expr

stmt :: Parser Stmt
stmt = whiteSpace >> p <* eof
  where
    p :: Parser Stmt
    p = Seq <$> semiSep1 stmt1
    stmt1 = do { v <- identifier;
                 reserved ":=";
                 e <- expr;
                 return (v := e)
               }
      <|> (reserved "if" *>
          (If <$> expr <*> (reserved "then" *> stmt) <*> (reserved "else" *> stmt)))
      <|> (reserved "while" *> (While <$> expr <*> (reserved "do" *> stmt)))
      <|> (reserved "print" *> (Print <$> expr))


parseString :: String -> Either ParseError Stmt
parseString = parse (stmt <* eof) ""

parseFile :: FilePath -> IO (Either ParseError Stmt)
parseFile = parseFromFile (stmt <* eof)
