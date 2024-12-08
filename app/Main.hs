{-# LANGUAGE LambdaCase #-}
module Main (main) where

import System.Environment
import System.IO
import System.Exit
import Utils
import ASTParser
import ASTEval
import Parser
import AST
import Control.Monad

getStdin :: IO String
getStdin = isEOF >>= \eof ->
    if eof
      then pure ""
      else do
        c <- getChar
        cs <- getStdin
        return (c : cs)

executeLines :: Env -> [Expr] -> IO ()
executeLines _ [] = pure ()
executeLines env (x:xs) = do
  case runState (eval x []) env of
    Left err -> putStrLn err >> exitWith (ExitFailure 84)
    Right (res, env') -> do
      if (show res /= "") then putStrLn (show res) else pure ()
      executeLines env' xs

main :: IO ()
main = do
  args <- getArgs
  fileInput <- case args of
    [] -> getStdin
    [name] -> (tryReadFile name) >>= \case
      Right file -> pure file
      Left err -> putStrLn err >> exitWith (ExitFailure 84)
    _ -> printHelp >> exitWith (ExitFailure 84)
  pure (runParser (sepBy ws exprP) fileInput) >>= \case
    Nothing -> putStrLn "parseError: invalid syntax" >> exitWith (ExitFailure 84)
    Just (_, expressions) -> executeLines [] expressions 
