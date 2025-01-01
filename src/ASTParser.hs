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
lispP = only $ ws *> sepBy ws (funcP <|> blockP <|> statementP) <* ws

statementP' :: Parser Expr -> Parser Expr
statementP' p = p <* ws <* charP ';'

statementP :: Parser Expr
statementP =
  statementP'
    ( inlineP
        <|> constP
        <|> applyP
        <|> lamP
        <|> defineP
        <|> listP
        <|> varP
    )

blockP :: Parser Expr
blockP = do
  _ <- charP '{' <* ws
  exprs <- sepBy ws statementP
  _ <- ws <* charP '}'
  return $ Block exprs

exprP :: Parser Expr
exprP =
  inlineP <|> exprP'

exprP' :: Parser Expr
exprP' =
  constP
    <|> applyP
    <|> lamP
    <|> defineP
    <|> listP
    <|> funcP
    <|> blockP
    <|> ifP
    <|> varP

constP :: Parser Expr
constP =
  Number <$> integerP
    <|> (Boolean . (== "true") <$> (stringP "true" <|> stringP "false"))

listP :: Parser Expr
listP = List <$> (charP '[' *> ws *> sepBy (ws *> charP ',' <* ws) exprP <* ws <* charP ']')

keywords :: [String]
keywords = ["define", "lambda", "if", "+", "-", "*", "div", "==", "!=", ">", "<"]

varP :: Parser Expr
varP = do
  str <- wordP
  if str `elem` keywords then empty else return $ Var str

inlineP :: Parser Expr
inlineP = ternaryP <|> inlineP'

inlineP' :: Parser Expr
inlineP' = booleanOpP <|> inlineP''

inlineP'' :: Parser Expr -- Inline expressions except for boolean operations
inlineP'' = addSubP <|> inlineP'''

inlineP''' :: Parser Expr -- Inline expressions except for boolean and arithmetic operations
inlineP''' = mulDivP <|> inlineP''''

inlineP'''' :: Parser Expr -- Inline expressions except for boolean, arithmetic, and multiplication/division operations
inlineP'''' = charP '(' *> ws *> exprP <* ws <* charP ')' <|> exprP'

ternaryP :: Parser Expr
ternaryP = do
  cond <- inlineP' <* ws <* charP '?' <* ws
  e1 <- exprP <* ws <* charP ':' <* ws
  Ternary cond e1 <$> exprP

booleanOpP :: Parser Expr
booleanOpP = do
  e1 <- inlineP'' <* ws
  con <- (Eq <$ stringP "==") <|> (Neq <$ stringP "!=") <|> (Gt <$ charP '>') <|> (Lt <$ charP '<')
  _ <- ws
  con e1 <$> inlineP'

addSubP :: Parser Expr
addSubP = do
  e1 <- inlineP''' <* ws
  con <- (Add <$ charP '+') <|> (Sub <$ charP '-')
  _ <- ws
  con e1 <$> inlineP''

mulDivP :: Parser Expr
mulDivP = do
  e1 <- inlineP'''' <* ws
  con <- (Mul <$ charP '*') <|> (Div <$ stringP "//")
  _ <- ws
  con e1 <$> inlineP'''

defineP :: Parser Expr
defineP = charP '(' *> ws *> stringP "define" *> notNull ws *> defineP' <* ws <* charP ')'

defineP' :: Parser Expr
defineP' = Define <$> (wordP <* notNull ws) <*> exprP

lamP :: Parser Expr
lamP = do
  _ <- charP '(' *> ws
  ids <- (singleParamP <|> multiParamP) <* ws <* stringP "=>" <* ws
  Lam ids <$> exprP <* ws <* charP ')'
 where
  singleParamP = do
    i <- wordP
    return [i]
  multiParamP = do
    _ <- charP '(' <* ws
    ids <- sepBy (ws *> charP ',' <* ws) wordP
    _ <- ws <* charP ')'
    return ids

applyP :: Parser Expr
applyP = do
  f <- varP <|> lamP
  _ <- ws <* charP '(' <* ws
  params <- sepBy (ws *> charP ',' <* ws) exprP
  _ <- ws *> charP ')'
  return $ Apply f params

funcP :: Parser Expr
funcP = do
  _ <- stringP "fn" *> notNull ws
  f <- wordP <* ws
  _ <- charP '(' <* ws
  ids <- sepBy (ws *> charP ',' <* ws) wordP
  _ <- ws <* charP ')' <* ws
  Define f . Lam ids <$> blockP

ifP :: Parser Expr
ifP = do
  _ <- stringP "if" *> notNull ws
  cond <- charP '(' *> ws *> exprP <* ws <* charP ')'
  e1 <- ws *> exprP <* ws
  elifs <- many $ do
    _ <- stringP "elif" *> notNull ws
    cond' <- charP '(' *> ws *> exprP <* ws <* charP ')'
    e' <- ws *> exprP <* ws
    return (cond', e')
  e2 <- optional $ do
    _ <- stringP "else" *> notNull ws
    exprP
  return $ If cond e1 elifs e2
