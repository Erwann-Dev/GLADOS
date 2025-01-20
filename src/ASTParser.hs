module ASTParser (exprP, variableInitializationP) where

import AST
import Control.Applicative
import Data.Char
import Parser

exprP :: Parser Node
exprP = inlineP <|> exprP'

exprP' :: Parser Node
exprP' =
  integerValueP
    <|> floatValueP
    <|> arrayAccessP
    <|> arrayValueP
    <|> variableInitializationP
    <|> blockP
    <|> printP
    <|> returnP
    <|> ifP
    <|> whileP
    <|> forP
    <|> syscallP
    <|> notOperatorP
    <|> sizeofP
    <|> referenceP
    <|> dereferenceP
    <|> enumDeclarationP
    <|> structDeclarationP
    <|> functionDeclarationP
    <|> functionCallP
    <|> structInitializationP
    <|> enumElementP
    <|> structElementP
    <|> castToTypeP
    <|> castToIdentifierP
    <|> identifierP

printP :: Parser Node
printP = do
  _ <- stringP "print" <* ws
  Print <$> wrapP '(' ')' exprP

keywords :: [String]
keywords =
  [ "u8"
  , "u16"
  , "u32"
  , "i8"
  , "i16"
  , "i32"
  , "f32"
  , "void"
  , "mut"
  , "ptr"
  , "return"
  , "not"
  , "or"
  , "and"
  , "enum"
  , "struct"
  , "if"
  , "else"
  , "while"
  , "for"
  , "as"
  , "sizeof"
  , "syscall"
  ]

symbolP :: Parser Symbol
symbolP = do
  id' <- ws *> spanP (\c -> isAlpha c || isDigit c || c == '_') <* ws
  if null id' || id' `elem` keywords || head id' `elem` ('_' : ['0' .. '9'])
    then empty
    else pure id'

wrapP :: Char -> Char -> Parser a -> Parser a
wrapP c1 c2 p = charP c1 *> ws *> p <* ws <* charP c2

typeP :: Parser Type
typeP = ptrTypeP <|> typeP'

basicTypeP :: Parser BasicType
basicTypeP =
  (U8 <$ stringP "u8")
    <|> (U16 <$ stringP "u16")
    <|> (U32 <$ stringP "u32")
    <|> (I8 <$ stringP "i8")
    <|> (I16 <$ stringP "i16")
    <|> (I32 <$ stringP "i32")
    <|> (F32 <$ stringP "f32")
    <|> (Void <$ stringP "void")

typeP' :: Parser Type
typeP' =
  flip Type
    <$> ((True <$ stringP "mut" <* notNull ws) <|> pure False)
    <*> basicTypeP

ptrTypeP :: Parser Type
ptrTypeP =
  flip Type
    <$> (stringP "ptr" *> (True <$ charP '?' <|> pure False))
    <*> (Pointer <$> (notNull ws *> typeP))

-- Algebraic value parsing

maybeNegateP :: (Num a) => Parser (a -> a)
maybeNegateP = do
  signs <- spanP $ \c -> c == '+' || c == '-'
  pure $
    if even $ (length . filter (== '-')) signs
      then id
      else negate

floatValueP :: Parser Node
floatValueP = do
  maybeNegate <- maybeNegateP
  separated <- (,) <$> spanP isDigit <* optionalP (charP '.') <*> spanP isDigit
  absVal <- case separated of
    ("", "") -> empty
    (dig, "") -> pure $ read dig
    ("", dec) -> pure $ read dec / (10 ^ length dec)
    (dig, dec) -> pure $ read dig + read dec / (10 ^ length dec)
  return $ FloatV $ maybeNegate absVal

integerValueP :: Parser Node
integerValueP = do
  maybeNegate <- maybeNegateP
  IntV . maybeNegate . read <$> notNull (spanP isDigit)

arrayValueP :: Parser Node
arrayValueP =
  ArrayV <$> wrapP '[' ']' (sepBy (ws *> charP ',' <* ws) exprP)

-- Expression Parsing

identifierP :: Parser Node
identifierP = do
  str <- symbolP
  if str `elem` keywords then empty else return $ Identifier str

variableInitializationP :: Parser Node
variableInitializationP = do
  varType <- typeP
  name <- ws *> symbolP <* ws <* charP '='
  value <- ws *> exprP
  return $ VarDef name varType value

blockP :: Parser Node
blockP = flip Block True <$> wrapP '{' '}' (sepBy ws exprP)

returnP :: Parser Node
returnP = Return <$> (stringP "return" *> notNull ws *> exprP)

conditionalBodyP :: Parser Node
conditionalBodyP = ConditionalBody <$> wrapP '{' '}' (sepBy ws exprP)

ifP :: Parser Node
ifP = do
  _ <- stringP "if"
  condition <- (ws *> wrapP '(' ')' exprP) <|> (notNull ws *> exprP <* notNull ws)
  thenBranch <- ws *> conditionalBodyP
  elseBranch <- optionalP $ ws *> stringP "else" *> ((notNull ws *> ifP) <|> (ws *> conditionalBodyP))
  return $ If condition thenBranch elseBranch

whileP :: Parser Node
whileP = do
  _ <- stringP "while"
  condition <- (ws *> wrapP '(' ')' exprP) <|> (notNull ws *> exprP <* notNull ws)
  body <- ws *> conditionalBodyP
  return $ While condition body

forP :: Parser Node
forP = do
  _ <- stringP "for" <* ws <* charP '('
  initialization <- ws *> optionalP exprP <* ws <* charP ';'
  condition <- ws *> exprP <* ws <* charP ';'
  increment <- ws *> optionalP exprP <* ws <* charP ')'
  body <- ws *> conditionalBodyP
  return $ For initialization condition increment body

-- u8 addu8 (u8 a , u8 b ) a + b
functionDeclarationP :: Parser Node
functionDeclarationP = do
  returnType <- typeP
  name <- notNull ws *> symbolP <* ws <* charP '(' <* ws
  parameters <- sepBy (ws *> charP ',' <* ws) parameterP <* ws <* charP ')'
  body <- ws *> exprP
  return $ FunctionDeclaration returnType name parameters body
 where
  parameterP = (,) <$> typeP <*> (notNull ws *> symbolP)

functionCallP :: Parser Node
functionCallP =
  FunctionCall
    <$> (identifierP <* ws)
    <*> wrapP '(' ')' (sepBy (ws *> charP ',' <* ws) exprP)

enumDeclarationP :: Parser Node
enumDeclarationP =
  EnumDeclaration
    <$> (stringP "enum" *> notNull ws *> symbolP <* ws)
    <*> wrapP '{' '}' (sepBy (ws *> charP ',' <* ws) identifierP)

structDeclarationP :: Parser Node
structDeclarationP =
  StructDeclaration
    <$> (stringP "struct" *> notNull ws *> symbolP <* ws)
    <*> wrapP '{' '}' (sepBy (charP ',') structFieldP)
 where
  structFieldP = (,) <$> typeP <*> (notNull ws *> symbolP)

structInitializationP :: Parser Node
structInitializationP = do
  name <- ws *> identifierP <* ws
  fields <- wrapP '{' '}' $ sepBy (ws *> charP ',' <* ws) structFieldP
  return $ StructInitialization name fields
 where
  structFieldP = (,) <$> identifierP <* ws <* charP ':' <* ws <*> exprP

enumElementP :: Parser Node
enumElementP = EnumElement <$> identifierP <* charP ':' <*> identifierP

structElementP :: Parser Node
structElementP = StructElement <$> identifierP <*> (charP '.' *> exprP)

castToTypeP :: Parser Node
castToTypeP = CastToType <$> wrapP '<' '>' typeP <*> (ws *> exprP)

castToIdentifierP :: Parser Node
castToIdentifierP = CastToIdentifier <$> wrapP '<' '>' identifierP <*> (ws *> exprP)

notOperatorP :: Parser Node
notOperatorP = NotOp <$> (stringP "not" *> notNull ws *> exprP)

sizeofP :: Parser Node
sizeofP =
  (stringP "sizeof" *> charP '(' *> ws)
    *> ( (SizeofType <$> typeP)
          <|> (SizeofExpr <$> exprP)
       )
    <* (ws <* charP ')')

referenceP :: Parser Node
referenceP = Reference <$> (charP '&' *> exprP)

dereferenceP :: Parser Node
dereferenceP = Reference <$> (charP '*' *> exprP)

arrayAccessP :: Parser Node
arrayAccessP = ArrayAccess <$> (identifierP <|> arrayValueP) <*> wrapP '[' ']' exprP

syscallP :: Parser Node
syscallP = do
  functionCall <- functionCallP
  case functionCall of
    FunctionCall (Identifier "syscall") args -> return $ Syscall args
    _ -> empty

inlineP :: Parser Node
inlineP = assignmentP <|> logicalGateP <|> inlineP'

inlineP' :: Parser Node
inlineP' = booleanOpP <|> inlineP''

inlineP'' :: Parser Node -- Inline expressions except for boolean operations
inlineP'' = addSubP <|> inlineP'''

inlineP''' :: Parser Node -- Inline expressions except for boolean and arithmetic operations
inlineP''' = mulDivP <|> inlineP''''

inlineP'''' :: Parser Node -- Inline expressions except for boolean, arithmetic, and multiplication/division operations
inlineP'''' = charP '(' *> ws *> exprP <* ws <* charP ')' <|> exprP'

assignmentP :: Parser Node
assignmentP = do
  id' <- symbolP <* ws
  con <-
    (AddEqOp <$ charP '+' <* ws <* charP '=')
      <|> (SubEqOp <$ charP '-' <* ws <* charP '=')
      <|> (MulEqOp <$ charP '*' <* ws <* charP '=')
      <|> (DivEqOp <$ charP '/' <* ws <* charP '=')
      <|> (VarAssign <$ stringP "=")
  con id' <$> (ws *> exprP)

logicalGateP :: Parser Node
logicalGateP = do
  e1 <- inlineP' <* ws
  con <- (AndOp <$ stringP "and") <|> (OrOp <$ stringP "or")
  _ <- ws
  con e1 <$> exprP

booleanOpP :: Parser Node
booleanOpP = do
  e1 <- inlineP'' <* ws
  con <-
    (EqOp <$ charP '=' <* ws <* charP '=')
      <|> (NeqOp <$ charP '!' <* ws <* charP '=')
      <|> (GeqOp <$ charP '>' <* ws <* charP '=')
      <|> (LeqOp <$ charP '<' <* ws <* charP '=')
      <|> (GtOp <$ charP '>')
      <|> (LtOp <$ charP '<')
  con e1 <$> (ws *> inlineP')

addSubP :: Parser Node
addSubP = do
  e1 <- inlineP''' <* ws
  con <- (AddOp <$ charP '+') <|> (SubOp <$ charP '-')
  con e1 <$> (ws *> inlineP'')

mulDivP :: Parser Node
mulDivP = do
  e1 <- inlineP'''' <* ws
  con <-
    (MulOp <$ charP '*')
      <|> (DivOp <$ stringP "/")
      <|> (ModOp <$ stringP "%")
  con e1 <$> (ws *> inlineP''')
