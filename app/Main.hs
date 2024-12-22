{-# LANGUAGE LambdaCase #-}

module Main (main) where

import AST
import ASTEval
import ASTParser
import Control.Monad
import Parser
import System.Environment
import System.Exit
import System.IO
import Utils

getStdin :: IO String
getStdin =
  isEOF >>= \eof ->
    if eof
      then pure ""
      else do
        c <- getChar
        cs <- getStdin
        return (c : cs)

executeLines :: Env -> [Expr] -> IO ()
executeLines _ [] = pure ()
executeLines env (x : xs) = do
  case runState (eval x []) env of
    Left err -> putStrLn err >> exitWith (ExitFailure 84)
    Right (res, env') -> do
      when (show res /= "") $ print res
      executeLines env' xs

main :: IO ()
main = do
  args <- getArgs
  fileInput <- case args of
    [] -> getStdin
    [name] ->
      tryReadFile name >>= \case
        Right file -> pure file
        Left err -> putStrLn err >> exitWith (ExitFailure 84)
    _ -> printHelp >> exitWith (ExitFailure 84)
  tryEval (runParser lispP fileInput)
 where
  tryEval Nothing = putStrLn "parseError: invalid syntax" >> exitWith (ExitFailure 84)
  tryEval (Just (_, expressions)) = executeLines [] expressions
