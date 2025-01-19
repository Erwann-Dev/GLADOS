-- | The 'Parser' module provides a simple parser combinator library for
-- building parsers for different types of input.
module Parser
  ( -- * Data Types
    Parser (..),

    -- * Basic Parsers
    charP,
    stringP,
    spanP,
    notNull,
    only,
    ws,

    -- * Combinators
    sepBy,
    optionalP,

    -- * Advanced Parsers
    lookAhead,
    manyTill,
    satisfyP,
  )
where

import Control.Applicative (Alternative (empty, many, (<|>)))
import Data.Char (isSpace)

-- | A 'Parser' is a function that takes an input string
-- and returns a 'Maybe' value consisting of the remaining input string and the
-- parsed result. If the parser fails, it returns 'Nothing'.
newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

-- | 'Functor' instance for 'Parser'. Allows mapping a function over the result of a parser.
instance Functor Parser where
  fmap f (Parser p) =
    Parser $ \input -> do
      (input', x) <- p input
      Just (input', f x)

-- | 'Applicative' instance for 'Parser'. Provides the ability to apply functions
-- to parsed values.
instance Applicative Parser where
  pure x = Parser $ \input -> Just (input, x)
  (Parser p1) <*> (Parser p2) =
    Parser $ \input -> do
      (input', f) <- p1 input
      (input'', a) <- p2 input'
      Just (input'', f a)

-- | 'Alternative' instance for 'Parser'. Allows the combination of parsers in a choice.
instance Alternative Parser where
  empty = Parser $ \_ -> Nothing
  (Parser p1) <|> (Parser p2) =
    Parser $ \input -> p1 input <|> p2 input

-- | 'Monad' instance for 'Parser'. Enables sequencing of parsers where the output
-- of one parser can be used as input to another.
instance Monad Parser where
  return = pure
  (Parser p) >>= f =
    Parser $ \input -> do
      (input', x) <- p input
      runParser (f x) input'

-- | 'charP' parses a single character that matches the input character.
--
-- It succeeds if the input string starts with the given character, and returns the
-- remaining string along with the said character. If the first character does not
-- match, the parser fails.
--
-- @
-- charP \'a\' "abc" == Just ("bc", \'a\')
-- charP \'b\' "abc" == Nothing
-- @
charP :: Char -> Parser Char
charP x = Parser f
  where
    f (y : ys)
      | y == x = Just (ys, x)
      | otherwise = Nothing
    f [] = Nothing

-- | 'optionalP' allows a parser to succeed even if the input does not match.
-- If the parser fails, it returns the input string as is.
-- If the parser succeeds, it returns the remaining input along with the parsed value.
--
-- @
-- optionalP (charP \'a\') "abc" == Just ("bc", Just \'a\')
-- optionalP (charP \'b\') "abc" == Just ("abc", Nothing)
-- @
optionalP :: Parser a -> Parser (Maybe a)
optionalP p = Just <$> p <|> pure Nothing

-- | 'stringP' parses a string of characters. It returns the matched string if all
-- characters match sequentially.
--
-- It applies 'charP' to each character in the string and returns the remaining
-- input along with the matched string.
--
-- @
-- stringP "abc" "abc" == Just ("", "abc")
-- stringP "abc" "abcd" == Just ("d", "abc")
-- stringP "abc" "ab" == Nothing
-- @
stringP :: String -> Parser String
stringP = sequenceA . map charP

-- | 'spanP' parses a sequence of characters that satisfy a given predicate.
--
-- It succeeds if the input string contains a prefix of characters that match the predicate,
-- and returns the matched substring along with the remaining input.
--
-- @
-- spanP isDigit "123abc" == Just ("abc", "123")
-- spanP isAlpha "abc123" == Just ("123", "abc")
-- spanP isDigit "abc" == Just ("abc", "")
-- @
spanP :: (Char -> Bool) -> Parser String
spanP f =
  Parser $ \input ->
    let (token, rest) = span f input
     in Just (rest, token)

-- | 'notNull' ensures that the result of a parser is not an empty list.
--
-- If the parser produces an empty list, it fails. Otherwise, it returns the
-- parsed value as usual.
--
-- @
-- notNull (spanP isDigit) "1abc" == Just ("abc", "1")
-- notNull (spanP isDigit) "abc" == Nothing
-- @
notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
  Parser $ \input -> do
    (input', xs) <- p input
    if null xs
      then Nothing
      else Just (input', xs)

-- | 'only' ensures that the input is fully consumed by the parser.
--
-- If the parser consumes the entire input, it returns the parsed value. Otherwise,
-- it fails.
--
-- @
-- only (charP \'a\') \"a\" == Just (\"\", \'a\')
-- only (charP \'a\') \"ab\" == Nothing
-- @
only :: Parser a -> Parser a
only (Parser p) =
  Parser $ \input -> do
    (input', x) <- p input
    if null input'
      then Just (input', x)
      else Nothing

-- | 'ws' parses whitespace characters (spaces, tabs, etc.).
--
-- It returns the matched whitespace string and the remaining input.
--
-- @
-- ws "   abc" == Just ("abc", "   ")
-- ws "abc" == Just ("abc", "")
-- @
ws :: Parser String
ws = spanP isSpace

-- | 'sepBy' parses a list of elements separated by a given separator.
--
-- It allows the separator to appear zero or more times between elements.
--
-- @
-- sepBy (charP \',\') (stringP "ab") "ab,ab,ab" == Just ("", ["ab", "ab", "ab"])
-- sepBy (charP \',\') (stringP "ab") "ab,cd,ef" == Just (",cd,ef", ["ab"])
-- sepBy (charP \',\') (stringP "ab") "cd,ef,gh" == Just ("cd,ef,gh", [])
-- @
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

-- | 'lookAhead' parses a value without consuming any input.
--
-- It returns the parsed value without modifying the input.
--
-- @
-- lookAhead (charP \'a\') \"abc\" == Just (\"abc\", \'a\')
-- lookAhead (charP \'b\') \"abc\" == Nothing
-- @
lookAhead :: Parser a -> Parser a
lookAhead (Parser p) = Parser $ \input -> case p input of
  Nothing -> Nothing
  Just (_, x) -> Just (input, x)

-- | 'satisfyP' parses a single character that satisfies a given predicate.
--
-- It succeeds if the input string starts with a character that satisfies the predicate,
-- and returns the remaining string along with the matched character. If the first character
-- does not satisfy the predicate, the parser fails.
--
-- @
-- satisfyP isDigit "123abc" == Just ("23abc", \'1\')
-- satisfyP isDigit "abc" == Nothing
-- @
satisfyP :: (Char -> Bool) -> Parser Char
satisfyP pred' = Parser f
  where
    f (x : xs) | pred' x = Just (xs, x)
    f _ = Nothing

-- | 'manyTill' parses zero or more occurrences of a parser until another parser succeeds.
--
-- It applies the first parser repeatedly until the second parser succeeds, and returns
-- the list of parsed values. Returns Nothing if the second parser never succeeds.
--
-- @
-- manyTill (charP \'a\') (charP \'b\') \"aaab\" == Just (\"\", \"aaa\")
-- manyTill (charP \'a\') (charP \'b\') \"aaabcc\" == Just (\"cc\", \"aaa\")
-- manyTill (charP \'a\') (charP \'b\') \"aaa\" == Nothing
-- @
manyTill :: Parser a -> Parser b -> Parser [a]
manyTill p end = go
  where
    go = ([] <$ end) <|> ((:) <$> p <*> go)
