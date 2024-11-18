{-
-- EPITECH PROJECT, 2024
-- Created by micka
-- File description:
-- Utils.hs
-}

module Utils (exErr, tryReadFile, printHelp) where

import System.Exit
import System.IO
import System.IO.Error
import Control.Exception

exErr:: String -> IO ()
exErr s = hPutStrLn stderr ("glados: " ++ s) >> exitWith (ExitFailure 84)

printHelp :: IO ()
printHelp =
    putStrLn "USAGE: ./mypandoc -i ifile -f oformat [-o ofile] [-e iformat]" >>
    putStrLn "     ifile        path to the file to convert" >>
    putStrLn "     oformat       output format (xml, json, markdown)" >>
    putStrLn "     ofile        path to the output file" >>
    putStrLn "     iformat       input format (xml, json, markdown)" >>
    exitWith (ExitFailure 84)

handleFileError :: IOError -> Maybe String
handleFileError er
  | isDoesNotExistError er = Just "fileError: does not exist"
  | isPermissionError   er = Just "fileError: permission denied"
  | otherwise              = Just "fileError: Couldn't open file"

tryReadFile :: String -> IO String
tryReadFile path = do
        eitherExceptionFile <- tryJust handleFileError (readFile path)
        case eitherExceptionFile of
           Left  err  -> exErr err >> return ""
           Right file -> return file
