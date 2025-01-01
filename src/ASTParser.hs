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
lispP = only $ ws *> sepBy ws exprP <* ws

exprP :: Parser Expr
exprP =
  builtinP
    <|> constP
    <|> ifP
    <|> lamP
    <|> parseApply
    <|> defineP
    <|> listP
    <|> varP

constP :: Parser Expr
constP =
  Number <$> integerP
    <|> (Boolean . (== "#t") <$> (stringP "#t" <|> stringP "#f"))

listP :: Parser Expr
listP = List <$> (charP '(' *> ws *> sepBy ws exprP <* ws <* charP ')')

keywords :: [String]
keywords = ["define", "lambda", "if", "+", "-", "*", "div", "==", "!=", ">", "<"]

varP :: Parser Expr
varP = do
  str <- wordP
  if str `elem` keywords then empty else return $ Var str

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
defineP = charP '(' *> ws *> stringP "define" *> notNull ws *> (defineLamP <|> defineP') <* ws <* charP ')'

defineP' :: Parser Expr
defineP' = Define <$> (wordP <* notNull ws) <*> exprP

defineLamP :: Parser Expr
defineLamP = do
  _ <- charP '(' *> ws
  symbol <- wordP <* notNull ws
  ids <- sepBy ws wordP
  _ <- ws <* charP ')' <* notNull ws
  e <- exprP
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
