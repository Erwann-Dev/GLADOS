{-
-- EPITECH PROJECT, 2024
-- Created by micka
-- File description:
-- Utils.hs
-}

module Utils
  ( tryReadFile
  , printHelp
  ) where

import System.IO.Error
import Control.Exception

printHelp :: IO ()
printHelp =
    putStrLn "USAGE: ./glados [file]" >>
    putStrLn "     file        file to be converted (leave empty to read user input)"

handleFileError :: IOError -> Maybe String
handleFileError er
  | isDoesNotExistError er = Just "fileError: does not exist"
  | isPermissionError   er = Just "fileError: permission denied"
  | otherwise              = Just "fileError: Couldn't open file"

tryReadFile :: String -> IO (Either String String)
tryReadFile path = do
        eitherExceptionFile <- tryJust handleFileError (readFile path)
        case eitherExceptionFile of
           Left  err  -> return $ Left err
           Right file -> return $ Right file
