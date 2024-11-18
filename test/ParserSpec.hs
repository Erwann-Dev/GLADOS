{-
-- EPITECH PROJECT, 2024
-- Created by micka
-- File description:
-- ParserSpec.hs
-}

module ParserSpec (spec) where

import Test.Hspec
import Parser

spec :: Spec
spec = do
    describe "Parser tests" $ do
        it "parses a certain string" $ do
            let str = "Hello World"
            runParser (stringP "Hello") str `shouldBe` Just (" World",  "Hello")

