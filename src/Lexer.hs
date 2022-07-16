-- |

module Lexer
  (
    names,
    opNames,
    identifier,
    symbol,
    reserved,
    reservedOp,
    parens,
    brackets,
    commaSep,
    integer,
    whiteSpace,
    semiSep1,
    lexer,
  )
where


import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language

import Control.Monad.IO.Class (MonadIO)
import Text.Pretty.Simple (CheckColorTty (..),
                           OutputOptions (..),
                           defaultOutputOptionsNoColor,
                           pPrintOpt)

names :: [String]
names = words "true false nil while do if then else let var print fun return"

opNames :: [String]
opNames = words "! - == != / < <= > >= + - * / && || :="

lexer = Token.makeTokenParser emptyDef
  {
    Token.commentStart = "/*",
    Token.commentEnd = "*/",
    Token.commentLine = "//",
    Token.identStart = letter,
    Token.identLetter = alphaNum <|> char '_' <|> char '\'',
    Token.reservedNames = names,
    Token.reservedOpNames = opNames
  }

pPrint :: (MonadIO m, Show a) => a -> m ()
pPrint = pPrintOpt CheckColorTty $
  defaultOutputOptionsNoColor
  {
    outputOptionsIndentAmount = 2,
    outputOptionsCompact = True,
    outputOptionsCompactParens = True
  }

identifier = Token.identifier lexer
symbol = Token.symbol lexer
reserved = Token.reserved lexer
reservedOp = Token.reservedOp lexer
parens = Token.parens lexer
brackets = Token.brackets lexer
commaSep = Token.commaSep lexer
integer = Token.integer lexer
whiteSpace = Token.whiteSpace lexer
semiSep1 = Token.semiSep1 lexer
