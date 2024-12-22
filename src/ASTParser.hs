module ASTParser (
  lispP,
  exprP,
) where

import AST
import Control.Applicative
import Data.Char
import Parser

isValidChar :: Char -> Bool
isValidChar c = isAlpha c || c == '_' || c == '-' || isDigit c

wordP :: Parser String
wordP = do
  str <- notNull $ spanP isValidChar
  if isDigit (head str) then empty else return str

integerP :: Parser Int
integerP = Parser f
 where
  f ('-' : input) = runParser (negate . read <$> notNull (spanP isDigit)) input
  f input = runParser (read <$> notNull (spanP isDigit)) input

lispP :: Parser [Expr]
lispP = Parser $ \input -> do
  (input', res) <- runParser (ws *> sepBy ws exprP <* ws) input
  if input' == "" then Just (input', res) else Nothing

exprP :: Parser Expr
exprP =
  builtinP
    <|> constP
    <|> ifP
    <|> lamP
    <|> parseApply
    <|> defineP
    <|> parseList
    <|> varP

constP :: Parser Expr
constP =
  Number <$> integerP
    <|> (Boolean . (== "#t") <$> (stringP "#t" <|> stringP "#f"))

parseList :: Parser Expr
parseList = List <$> (charP '(' *> ws *> sepBy ws exprP <* ws <* charP ')')

varP :: Parser Expr
varP = Var <$> wordP

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
    <|> (Neq <$ stringP "!=")
    <|> (Gt <$ stringP ">")
    <|> (Lt <$ stringP "<")

ifP :: Parser Expr
ifP = do
  _ <- charP '(' *> stringP "if" *> notNull ws
  g <- exprP <* notNull ws
  e1 <- exprP <* notNull ws
  If g e1 <$> exprP <* ws <* charP ')'

defineP :: Parser Expr
defineP = defineLamP <|> defineP'

defineP' :: Parser Expr
defineP' = do
  _ <- charP '(' *> ws *> stringP "define" <* notNull ws
  symbol <- wordP <* notNull ws
  e <- exprP <* ws <* charP ')'
  return $ Define symbol e

defineLamP :: Parser Expr
defineLamP = do
  _ <- charP '(' *> ws *> stringP "define" <* notNull ws
  symbol <- wordP <* notNull ws
  ids <- charP '(' *> ws *> sepBy ws wordP <* ws <* charP ')' <* ws
  e <- exprP <* ws <* charP ')'
  return $ Define symbol (Lam ids e)

lamP :: Parser Expr
lamP = do
  _ <- charP '(' *> ws *> stringP "lambda" *> notNull ws
  ids <- charP '(' *> ws *> sepBy ws wordP <* ws <* charP ')' <* ws
  Lam ids <$> exprP <* ws <* charP ')'

parseApply :: Parser Expr
parseApply = do
  f <- stringP "%{" *> exprP
  _ <- charP ':'
  ins <- sepBy ws exprP
  _ <- charP '}'
  return $ Apply f ins
