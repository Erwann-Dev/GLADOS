module Main (main) where

import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.FilePath (takeFileName)

import AST
import ASTCompiler (compile)
import ASTParser (exprP)
import Bytecode (toBytecode, writeBytecodeToFile)
import Parser (runParser, sepBy, ws)
import Preprocessor (runPreprocessor)
import Utils (tryReadFile)

usage :: String
usage = "USAGE: ./glados [-o output] file"

replaceExtension :: String -> String -> String
replaceExtension filepath ext =
  let filename = takeFileName filepath
   in case break (== '.') filename of
        (name, _) -> name ++ ext

processArgs :: [String] -> (Maybe String, Maybe String) -> IO (Maybe (String, String))
processArgs [] (Nothing, _) = putStrLn usage >> exitWith (ExitFailure 84)
processArgs [] (Just input, Nothing) = return $ Just (input, replaceExtension input ".bc")
processArgs [] (Just input, Just output) = return $ Just (input, output)
processArgs ("-h" : _) _ = putStrLn usage >> exitSuccess
processArgs ("-o" : output : xs) (mFile, _) = processArgs xs (mFile, Just output)
processArgs (input : xs) (Nothing, mOutput) = processArgs xs (Just input, mOutput)
processArgs _ _ = putStrLn usage >> exitWith (ExitFailure 84)

main :: IO ()
main = do
  args <- getArgs
  Just (inFile, outFile) <- processArgs args (Nothing, Nothing)
  fileInput <- do
    result <- tryReadFile inFile Nothing
    case result of
      Right file -> pure file
      Left err -> putStrLn err >> exitWith (ExitFailure 84)
  preprocessResult <- runPreprocessor fileInput
  case preprocessResult of
    Nothing -> do
      putStrLn "preprocessorError: invalid syntax"
      exitWith (ExitFailure 84)
    Just result -> case runParser (ws *> sepBy ws exprP <* ws) result of
      Nothing -> do
        putStrLn "parseError: invalid syntax"
        exitWith (ExitFailure 84)
      Just (rest, ast) ->
        if rest /= ""
          then do
            putStrLn "parseError: invalid syntax"
            exitWith (ExitFailure 84)
          else do
            case compile $ Block ast True of
              Left err -> do
                putStrLn $ "compilationError: " ++ show err
                exitWith (ExitFailure 84)
              Right instructions -> do
                let bytecode = toBytecode instructions
                writeResult <- writeBytecodeToFile outFile bytecode
                case writeResult of
                  Left err -> do
                    putStrLn err
                    exitWith (ExitFailure 84)
                  Right _ -> exitSuccess
