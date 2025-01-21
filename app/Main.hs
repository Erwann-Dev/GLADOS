module Main (main) where

import AST
import ASTEval
import ASTParser (exprP)
import Parser
import Preprocessor (runPreprocessor)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Utils (tryReadFile)

main :: IO ()
main = do
  args <- getArgs
  fileInput <- case args of
    [name] -> do
      result <- tryReadFile name Nothing
      case result of
        Right file -> pure file
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
    _ -> do
      putStrLn "USAGE: ./glados [file]"
      exitWith (ExitFailure 84)
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
          else executeLines initialState ast

executeLines :: InterpreterState -> [Node] -> IO ()
executeLines _ [] = return ()
executeLines state (x : xs) = do
  -- Use runEval instead of runState and handle both error and state components
  case runEval (eval x) state of
    (Left err, _) -> putStrLn err >> exitWith (ExitFailure 84)
    (Right _, newState) -> 
      mapM_ print (printBuffer newState) >> 
      executeLines (newState {printBuffer = []}) xs
