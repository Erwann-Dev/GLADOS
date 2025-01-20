module Main (main) where

-- import Preprocessor (runPreprocessor)

import AST
import ASTEval
import ASTParser (exprP)
import Parser
import Preprocessor (runPreprocessor)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import Utils (tryReadFile)

-- import AST
-- import ASTEval
-- import ASTParser
-- import Control.Monad
-- import Parser
-- import System.Environment
-- import System.Exit
-- import System.IO
-- import Utils
-- getStdin :: IO String
-- getStdin =
--   isEOF >>= \eof ->
--     if eof
--       then pure ""
--       else do
--         c <- getChar
--         cs <- getStdin
--         return (c : cs)
-- repl :: IO ()
-- repl = do
--   putStrLn "Welcome to the LISP interpreter"
--   putStrLn "Type :q to quit"
--   repl' []
-- repl' :: Env -> IO ()
-- repl' env = do
--   line <- putStr "> " >> hFlush stdout >> getLine
--   case line of
--     ":q" -> putStrLn "Exited."
--     _ -> case runParser (only exprP) line of
--       Nothing -> putStrLn "parseError: invalid syntax" >> repl' env
--       Just (_, expression) -> do
--         case runState (eval expression) env of
--           Left err -> putStrLn ("*** ERROR : " ++ err) >> repl' env
--           Right (res, env') -> do
--             when (show res /= "") $ print res
--             repl' env'
-- main :: IO ()
-- main = do
--     args <- getArgs
--     fileInput <- case args of
--         -- [] -> getStdin
--         -- ["repl"] -> repl >> exitSuccess
--         [name] ->
--             tryReadFile name Nothing >>= \case
--                 Right file -> pure $ content file
--                 Left err -> putStrLn err >> exitWith (ExitFailure 84)
--         _ -> putStrLn "USAGE: ./glados [file]" >> exitWith (ExitFailure 84)
--     tryEval (runParser (ws *> sepBy ws exprP <* ws) fileInput)
--   where
--     tryEval Nothing = putStrLn "parseError: invalid syntax" >> exitWith (ExitFailure 84)
--     tryEval (Just (_, expressions)) = executeLines initialState expressions

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
  case runState (eval x) state of
    Left err -> putStrLn err >> exitWith (ExitFailure 84)
    Right (_, newState) -> mapM_ print (printBuffer newState) >> executeLines (newState {printBuffer = []}) xs
