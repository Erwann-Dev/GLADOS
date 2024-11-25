{-
-- EPITECH PROJECT, 2024
-- Created by micka
-- File description:
-- ParserSpec.hs
-}

module ParserSpec (spec) where

import Test.Hspec
import Parser
import Data.Char
import Control.Applicative

spec :: Spec
spec = do
    describe "Basic parser tests" $ do
        it "parses a char" $ do
            runParser (charP 'H') "Hello World" `shouldBe` Just ("ello World",  'H')

        it "does not parse a char" $ do
            runParser (charP 'T') "Hello World" `shouldBe` Nothing
            runParser (charP 'T') "" `shouldBe` Nothing

        it "parses a string" $ do
            runParser (stringP "Hello") "Hello World" `shouldBe` Just (" World",  "Hello")

        it "does not parse a string" $ do
            runParser (stringP "World") "Hello World" `shouldBe` Nothing

        it "parses a predicate" $ do
            runParser (spanP isDigit) "123Soleil" `shouldBe` Just ("Soleil",  "123")

        it "does not parse a predicate" $ do
            runParser (spanP (not . isDigit)) "123Soleil" `shouldBe` Just ("123Soleil", "")

        it "parses a special data structure" $ do
            let tupleParser = (,) <$> (charP '(' *> spanP isDigit <* charP ',') <*> (spanP isDigit <* charP ')')
            runParser tupleParser "(123,456)" `shouldBe` Just ("",  ("123", "456"))

        it "runs two different parsers" $ do
            let newParser = charP 'H' <|> charP 'W'
            runParser newParser "Hello" `shouldBe` Just ("ello",  'H')
            runParser newParser "World" `shouldBe` Just ("orld",  'W')
