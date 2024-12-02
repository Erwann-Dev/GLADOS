module ASTParser where

import AST
import Control.Applicative
import Data.Char
import Parser

wordP :: Parser String
wordP = notNull $ spanP isAlpha

integerP :: Parser Int
integerP = Parser f
 where
  f ('-' : input) = runParser (negate . read <$> notNull (spanP isDigit)) input
  f input = runParser (read <$> notNull (spanP isDigit)) input

lispP :: Parser Expr
lispP = Parser $ \input -> do
  (input', res) <- runParser exprP input
  if input' == "" then Just (input', res) else Nothing

exprP :: Parser Expr
exprP =
  builtinP
    <|> constP
    <|> parseList

constP :: Parser Expr
constP =
  Number <$> integerP
    <|> (Boolean . (== "#t") <$> (stringP "#t" <|> stringP "#f"))

parseList :: Parser Expr
parseList = List <$> (charP '(' *> ws *> sepBy ws exprP <* ws <* charP ')')

builtinP :: Parser Expr
builtinP = (parseBuiltin >>= parseArgs) <* ws <* charP ')'
 where
  parseBuiltin = charP '(' *> ws *> builtinP' <* ws
  parseArgs con = con <$> exprP <* notNull ws <*> exprP

builtinP' :: Parser (Expr -> Expr -> Expr)
builtinP' =
  (Add <$ charP '+')
    <|> (Sub <$ charP '-')
    <|> (Mul <$ charP '*')
    <|> (Div <$ stringP "div")
    <|> (Eq <$ stringP "==")

