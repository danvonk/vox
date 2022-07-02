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
    lexer,
  )
where


import Text.Parsec
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language

names = words "true false nil if then let var"
opNames = words "! - == != / < <= > >= + - * / && ||"

lexer = Token.makeTokenParser emptyDef
  {
    Token.commentStart = "/*",
    Token.commentEnd = "*/",
    Token.commentLine = "#",
    Token.identStart = letter,
    Token.identLetter = alphaNum <|> char '_' <|> char '\'',
    Token.reservedNames = names,
    Token.reservedOpNames = opNames
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
