module Main (main) where

import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitSuccess, exitWith)

import ASTVM (output, runVM)

-- Command line options
data Flag = Help
  deriving (Eq, Show)

-- Option descriptors
options :: [OptDescr Flag]
options =
  [ Option
      ['h']
      ["help"]
      (NoArg Help)
      "show help"
  ]

-- Usage info
usageInfo' :: String
usageInfo' = usageInfo header options
 where
  header = "Usage: ./glados-vm bytecode-file"

main :: IO ()
main = do
  args <- getArgs
  case getOpt Permute options args of
    (flags, [input], []) -> do
      when (Help `elem` flags) $ do
        putStrLn usageInfo'
        exitSuccess

      -- Read bytecode file
      bytecode <-
        BS.readFile input `catch` \e -> do
          putStrLn $ "Error reading file: " ++ show (e :: IOError)
          exitWith $ ExitFailure 84

      -- Run VM
      case runVM bytecode of
        Left err -> do
          putStrLn $ "vmError: " ++ err
          exitWith $ ExitFailure 84
        Right finalState -> do
          -- Print outputs in order
          mapM_ putStrLn $ reverse $ output finalState
          exitSuccess
    (_, _, errs) -> do
      putStrLn $ concat errs ++ usageInfo'
      exitWith $ ExitFailure 84
