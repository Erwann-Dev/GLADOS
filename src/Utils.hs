module Utils
  ( File (..),
    tryReadFile,
    -- printHelp,
  )
where

import Control.Exception
import System.FilePath ((</>))
import System.IO.Error

data File = File
  { path :: String,
    content :: String
  }

-- printHelp :: IO ()
-- printHelp =
--   putStrLn "USAGE: ./glados [file]"
--     >> putStrLn "     file        file to be converted (leave empty to read user input)"

handleFileError :: IOError -> Maybe String
handleFileError er
  | isDoesNotExistError er = Just "fileError: does not exist"
  | isPermissionError er = Just "fileError: permission denied"
  | otherwise = Just "fileError: Couldn't open file"

tryReadFile :: String -> Maybe String -> IO (Either String File)
tryReadFile filePath Nothing = do
  eitherExceptionFile <- tryJust handleFileError (readFile filePath)
  case eitherExceptionFile of
    Left err -> return $ Left err
    Right fileContent -> return $ Right $ File {path = filePath, content = fileContent}
tryReadFile filePath (Just relativeTo) = do
  eitherExceptionFile <- tryJust handleFileError (readFile $ relativeTo </> filePath)
  case eitherExceptionFile of
    Left err -> return $ Left err
    Right fileContent -> return $ Right $ File {path = filePath, content = fileContent}
