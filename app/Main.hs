module Main (main) where

import System.Exit
import System.Environment
import System.IO

checkTypeFile :: String -> Bool
checkTypeFile file_path = 
    let file_extension = reverse $ takeWhile (/= '.') $ reverse file_path
    in file_extension == "scm"

checkPathFile :: [String] -> IO (Maybe String)
checkPathFile args = case args of
    [] -> return Nothing
    [path] -> 
        if not $ checkTypeFile path then do
            hPutStrLn stderr "Error: wrong file extension"
            exitWith (ExitFailure 84)
        else return $ Just path
    _ -> do
        hPutStrLn stderr "Error: too many arguments"
        exitWith (ExitFailure 84)

tryReadFile :: String -> IO String
tryReadFile path = readFile path

main :: IO ()
main = do
    args <- getArgs
    filePath <- checkPathFile args
    case filePath of 
        Just path -> do
            contents <- tryReadFile path
            putStrLn contents
        Nothing -> do
            contents <- getContents
            putStrLn contents

