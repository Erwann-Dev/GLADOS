module ASTParser (expressionP) where

import AST
import Control.Applicative (Alternative (empty, many), (<|>))
import Data.Char (isDigit, isSpace)
import Parser

keywords :: [String]
keywords =
  [ "u8",
    "u16",
    "u32",
    "u64",
    "i8",
    "i16",
    "i32",
    "i64",
    "f32",
    "f64",
    "void",
    "mut",
    "ptr",
    "return",
    "not",
    "or",
    "and",
    "enum",
    "struct",
    "if",
    "else",
    "while",
    "for",
    "as",
    "sizeof",
    "syscall"
  ]

parseType' :: Bool -> Parser Type
parseType' isMutable =
  typeParser "u8" U8 isMutable
    <|> typeParser "u16" U16 isMutable
    <|> typeParser "u32" U32 isMutable
    <|> typeParser "u64" U64 isMutable
    <|> typeParser "i8" I8 isMutable
    <|> typeParser "i16" I16 isMutable
    <|> typeParser "i32" I32 isMutable
    <|> typeParser "i64" I64 isMutable
    <|> typeParser "f32" F32 isMutable
    <|> typeParser "f64" F64 isMutable
    <|> typeParser "void" Void isMutable
  where
    typeParser :: String -> BasicType -> Bool -> Parser Type
    typeParser str basicType isMutable' = do
      _ <- ws <* stringP str <* ws
      return $
        Type
          { basic_type = basicType,
            mutable = isMutable'
          }

immutableTypeP' :: Parser Type
immutableTypeP' = parseType' False

mutableTypeP' :: Parser Type
mutableTypeP' = ws *> stringP "mut" *> ws *> parseType' True

ptrTypeP' :: Parser Type
ptrTypeP' = mutablePtrTypeP <|> immutablePtrTypeP
  where
    mutablePtrTypeP = do
      inner_type <- ws *> stringP "ptr" *> ws *> charP '?' *> ws *> typeP'
      return $
        Type
          { basic_type = Pointer inner_type,
            mutable = True
          }
    immutablePtrTypeP = do
      inner_type <- ws *> stringP "ptr" *> ws *> typeP'
      return $
        Type
          { basic_type = Pointer inner_type,
            mutable = False
          }

typeP' :: Parser Type
typeP' = ptrTypeP' <|> mutableTypeP' <|> immutableTypeP'

floatValueP' :: Parser Node
floatValueP' = do
  signs <- ws *> spanP (\c -> c == '+' || c == '-' || isSpace c)
  digits <- ws *> spanP isDigit <* ws <* charP '.'
  decimalDigits <- ws *> spanP isDigit <* ws
  if null digits && null decimalDigits
    then empty
    else
      let finalSign = if even $ length $ filter (== '-') signs then 1 else -1
          wholeNum = if null digits then "0" else digits
          decimals = if null decimalDigits then "0" else decimalDigits
          val = finalSign * (read wholeNum + read decimals / (10 ^ length decimals))
       in return $ FloatValue val

integerValueP' :: Parser Node
integerValueP' = do
  signs <- ws *> spanP (\c -> c == '+' || c == '-' || isSpace c)
  digits <- ws *> spanP isDigit <* ws
  if null digits
    then empty
    else
      let finalSign = if even $ length $ filter (== '-') signs then 1 else -1
       in return $ IntegerValue $ finalSign * read digits

sizeOfTypeP' :: Parser Node
sizeOfTypeP' = do
  t <- ws *> stringP "sizeof" *> ws *> optional (charP '(' *> ws) *> typeP' <* optional (ws <* charP ')') <* ws
  return $ SizeofOfTypeOperator t

sizeOfExpressionP' :: Parser Node
sizeOfExpressionP' = do
  e <- ws *> stringP "sizeof" *> ws *> optional (charP '(' *> ws) *> expressionP <* optional (ws <* charP ')') <* ws
  return $ SizeofOfExpressionOperator e

identifierP :: Parser Node
identifierP = do
  id' <- ws *> spanP (\c -> c `elem` ['a' .. 'z'] || c `elem` ['A' .. 'Z'] || c `elem` ['0' .. '9'] || c == '_') <* ws
  if null id' || id' `elem` keywords || head id' `elem` ('_' : ['0' .. '9'])
    then empty
    else return $ Identifier id'

numericalValueP :: Parser Node
numericalValueP = floatValueP' <|> integerValueP'

arrayValueP :: Parser Node
arrayValueP = do
  values <- ws *> charP '[' *> sepBy (charP ',') expressionP <* charP ']' <* ws
  return $ ArrayValue values

variableInitializationP :: Parser Node
variableInitializationP = do
  type' <- ws *> typeP'
  id' <- ws *> identifierP <* ws <* charP '='
  value <- ws *> expressionP
  return $ VariableInitialization id' type' value

enumDeclarationP :: Parser Node
enumDeclarationP = do
  id' <- ws *> stringP "enum" *> ws *> identifierP <* ws <* charP '{'
  variants <- sepBy (charP ',') identifierP <* charP '}' <* ws
  return $ EnumDeclaration id' variants

structDeclarationP :: Parser Node
structDeclarationP = do
  id' <- ws *> stringP "struct" *> ws *> identifierP <* ws <* charP '{'
  fields <- sepBy (charP ',') structFieldP <* ws <* charP '}' <* ws
  return $ StructDeclaration id' fields
  where
    structFieldP = do
      t <- typeP'
      id' <- ws *> identifierP
      return (t, id')

enumElementP :: Parser Node
enumElementP = do
  enumId <- ws *> identifierP
  variant <- ws *> charP ':' *> ws *> charP ':' *> ws *> identifierP
  return $ EnumElement enumId variant

sizeOfP :: Parser Node
sizeOfP = sizeOfTypeP' <|> sizeOfExpressionP'

referenceP :: Parser Node
referenceP = do
  element <- ws *> charP '&' *> ws *> expressionP
  return $ ReferenceOperator element

blockP :: Parser Node
blockP = do
  expressions <- ws *> charP '{' *> many expressionP <* charP '}' <* ws
  return $ Block expressions

returnP :: Parser Node
returnP = do
  _ <- ws *> stringP "return"
  value <- ws *> expressionP
  return $ Return value

ifP :: Parser Node
ifP = do
  condition <- ws *> stringP "if" *> ws *> optional (charP '(' *> ws) *> expressionP <* optional (ws <* charP ')')
  thenBranch <- ws *> expressionP
  elseBranch <- optional (ws *> stringP "else" *> ws *> expressionP)
  return $ If condition thenBranch elseBranch

whileP :: Parser Node
whileP = do
  condition <- ws *> stringP "while" *> ws *> optional (charP '(' *> ws) *> expressionP <* optional (ws <* charP ')')
  body <- ws *> expressionP
  return $ While condition body

forP :: Parser Node
forP = do
  initialization <- ws *> stringP "for" *> ws *> charP '(' *> ws *> optional expressionP <* ws <* charP ','
  condition <- ws *> expressionP <* ws <* charP ','
  increment <- ws *> optional expressionP <* ws <* charP ')'
  body <- ws *> expressionP
  return $ For initialization condition increment body

-- u8 addu8 (u8 a , u8 b ) a + b
functionDeclarationP :: Parser Node
functionDeclarationP = do
  returnType <- ws *> typeP'
  name <- ws *> identifierP <* ws <* charP '('
  parameters <- ws *> sepBy (charP ',') parameterP <* ws <* charP ')'
  body <- ws *> expressionP
  return $ FunctionDeclaration returnType name parameters body
  where
    parameterP = do
      t <- typeP'
      id' <- ws *> identifierP
      return (t, id')

functionCallP :: Parser Node
functionCallP = do
  name <- ws *> identifierP <* ws <* charP '('
  arguments <- ws *> sepBy (charP ',') expressionP <* ws <* charP ')' <* ws
  return $ FunctionCall name arguments

structInitializationP :: Parser Node
structInitializationP = do
  structName <- ws *> identifierP <* ws <* charP '{'
  fields <- sepBy (charP ',') structFieldP <* ws <* charP '}' <* ws
  return $ StructInitialization structName fields
  where
    structFieldP = do
      id' <- identifierP <* ws <* charP ':'
      value <- ws *> expressionP
      return (id', value)

castToTypeP :: Parser Node
castToTypeP = do
  expr <- lookAhead $ manyTill (satisfyP (const True)) (stringP "as")
  _ <- stringP expr
  types <- many (ws *> stringP "as" *> ws *> typeP')
  if null types
    then empty
    else do
      case runParser expressionP expr of
        Just ("", e) -> return $ foldr CastToType e types
        _ -> empty

castToIdentifierP :: Parser Node
castToIdentifierP = do
  expr <- lookAhead $ manyTill (satisfyP (const True)) (stringP "as")
  _ <- stringP expr
  id' <- ws *> identifierP
  case runParser expressionP expr of
    Just ("", e) -> return $ CastToIdentifier e id'
    _ -> empty

andOperatorP :: Parser Node
andOperatorP = do
  firstExpr <- manyTill (satisfyP (const True)) (stringP "and")
  case runParser expressionP firstExpr of
    Just ("", e) -> do
      AndOperator e <$> expressionP
    _ -> empty

orOperatorP :: Parser Node
orOperatorP = do
  firstExpr <- manyTill (satisfyP (const True)) (stringP "or")
  case runParser expressionP firstExpr of
    Just ("", e) -> do
      OrOperator e <$> expressionP
    _ -> empty

structElementP :: Parser Node
structElementP = do
  left <- manyTill (satisfyP (const True)) (charP '.')
  case runParser expressionP left of
    Just ("", l) -> do
      right <- ws *> expressionP
      return $ StructElement l right
    _ -> empty

notOperatorP :: Parser Node
notOperatorP = NotOperator <$> (ws *> stringP "not" *> ws *> expressionP)

precedentArithmeticOperatorP' :: Parser Node
precedentArithmeticOperatorP' = do
  left <- lookAhead $ manyTill (satisfyP (const True)) (charP '*' <|> charP '/' <|> charP '%')
  operator <- stringP left *> (charP '*' <|> charP '/' <|> charP '%')
  right <- ws *> expressionP
  case runParser expressionP left of
    Just ("", left') -> case operator of
      '*' -> return $ MultiplyOperator left' right
      '/' -> return $ DivideOperator left' right
      '%' -> return $ ModuloOperator left' right
      _ -> empty
    _ -> empty

nonPrecedentArithmeticOperatorP' :: Parser Node
nonPrecedentArithmeticOperatorP' = do
  left <- lookAhead $ manyTill (satisfyP (const True)) (charP '+' <|> charP '-')
  operator <- stringP left *> (charP '+' <|> charP '-')
  right <- ws *> expressionP
  case runParser expressionP left of
    Just ("", left') -> case operator of
      '+' -> return $ PlusOperator left' right
      '-' -> return $ MinusOperator left' right
      _ -> empty
    _ -> empty

arithmeticOperatorP :: Parser Node
arithmeticOperatorP = precedentArithmeticOperatorP' <|> nonPrecedentArithmeticOperatorP'

dereferenceP :: Parser Node
dereferenceP = do
  e <- ws *> charP '*' *> ws *> expressionP
  return $ DereferenceOperator e

arrayAccessP :: Parser Node
arrayAccessP = do
  before <- manyTill (satisfyP (const True)) (charP '[')
  inside <- expressionP <* ws <* charP ']' <* ws
  case runParser expressionP before of
    Just ("", e) -> return $ ArrayAccess e inside
    _ -> empty

syscallP :: Parser Node
syscallP = do
  functionCall <- functionCallP
  case functionCall of
    FunctionCall (Identifier "syscall") args -> return $ Syscall args
    _ -> empty

assignmentOperatorsP :: Parser Node
assignmentOperatorsP = do
  left <- lookAhead $ manyTill (satisfyP (const True)) (charP '=' <|> (charP '+' <* ws <* charP '=') <|> (charP '-' <* ws <* charP '=') <|> (charP '*' <* ws <* charP '=') <|> (charP '/' <* ws <* charP '='))
  _ <- stringP left
  case runParser expressionP left of
    Just ("", l) -> do
      operator <- charP '=' <|> (charP '+' <* ws <* charP '=') <|> (charP '-' <* ws <* charP '=') <|> (charP '*' <* ws <* charP '=') <|> (charP '/' <* ws <* charP '=')
      right <- expressionP
      case operator of
        '=' -> return $ Assignment l right
        '+' -> return $ PlusEqualOperator l right
        '-' -> return $ MinusEqualOperator l right
        '*' -> return $ MultiplyEqualOperator l right
        '/' -> return $ DivideEqualOperator l right
        _ -> empty
    _ -> empty

comparaisonOperatorP :: Parser Node
comparaisonOperatorP = do
  left <- lookAhead $ manyTill (satisfyP (const True)) (charP '=' <|> charP '!' <|> charP '<' <|> charP '>')
  _ <- stringP left
  case runParser expressionP left of
    Just ("", l) -> do
      operatorChar0 <- charP '=' <|> charP '!' <|> charP '<' <|> charP '>'
      operatorChar1 <- ws *> optional (charP '=')
      case (operatorChar0, operatorChar1) of
        ('=', Just '=') -> EqualOperator l <$> expressionP
        ('!', Just '=') -> NotEqualOperator l <$> expressionP
        ('<', Just '=') -> LessThanOrEqualOperator l <$> expressionP
        ('>', Just '=') -> GreaterThanOrEqualOperator l <$> expressionP
        ('<', Nothing) -> LessThanOperator l <$> expressionP
        ('>', Nothing) -> GreaterThanOperator l <$> expressionP
        _ -> empty
    _ -> empty

parenthesesP :: Parser Node
parenthesesP = ws *> charP '(' *> ws *> expressionP <* ws <* charP ')' <* ws

leftRecursiveParsers :: Parser Node
leftRecursiveParsers =
  assignmentOperatorsP
    <|> comparaisonOperatorP
    <|> orOperatorP
    <|> andOperatorP
    <|> arithmeticOperatorP
    <|> castToTypeP
    <|> castToIdentifierP
    <|> structElementP
    <|> arrayAccessP

nonLeftRecursiveParsers :: Parser Node
nonLeftRecursiveParsers =
  enumElementP
    <|> structInitializationP
    <|> syscallP
    <|> functionCallP
    <|> variableInitializationP
    <|> functionDeclarationP
    <|> parenthesesP
    <|> arrayValueP
    <|> dereferenceP
    <|> referenceP
    <|> notOperatorP
    <|> sizeOfP
    <|> returnP
    <|> ifP
    <|> forP
    <|> whileP
    <|> enumDeclarationP
    <|> structDeclarationP
    <|> blockP
    <|> identifierP
    <|> numericalValueP

expressionP :: Parser Node
expressionP = leftRecursiveParsers <|> nonLeftRecursiveParsers
