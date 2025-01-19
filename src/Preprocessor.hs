module Preprocessor
  ( runPreprocessor,
  )
where

import Data.Char (chr, isSpace)
import qualified Data.Map as Map
import System.FilePath (takeDirectory)
import Text.Read (readMaybe)
import Utils (File (..), tryReadFile)

data PPState = PPState
  { defines :: Map.Map String (Maybe String),
    ifStack :: [Bool],
    currentFileDir :: String
  }

runPreprocessor :: File -> IO (Maybe String)
runPreprocessor file =
  maybe
    (return Nothing)
    ( \processed -> do
        result <- runPreProcessorDirectives processed (path file)
        return $ fmap (\r -> "{ " ++ r ++ " }") result
    )
    (runFormatter (content file))

runFormatter :: String -> Maybe String
runFormatter fileContent = convertStringsToCharArrays (removeEscapedLineReturns fileContent) >>= convertCharsToNumbers >>= stripComments

escapeSequences :: Map.Map Char Int
escapeSequences =
  Map.fromList
    [ ('0', 0),
      ('a', 7),
      ('b', 8),
      ('t', 9),
      ('n', 10),
      ('v', 11),
      ('f', 12),
      ('r', 13)
    ]

specialTokens :: String
specialTokens =
  ".,(){}[]&*+-/%:><=?;!"

tokenize :: String -> [String]
tokenize = go ""
  where
    go acc [] = [acc | not (null acc)]
    go acc (c : cs)
      | c `elem` specialTokens =
          [acc | not (null acc)] ++ [[c]] ++ go "" cs
      | isSpace c =
          if null acc
            then go "" cs
            else acc : go "" cs
      | otherwise = go (acc ++ [c]) cs

stripComments :: String -> Maybe String
stripComments str = stripBlockComments $ stripLineComments str

stripLineComments :: String -> String
stripLineComments str = case lines str of
  [] -> ""
  line -> unlines $ map processLine line
    where
      processLine "" = ""
      processLine ('/' : '/' : _) = ""
      processLine (c : rest) = c : processLine rest

stripBlockComments :: String -> Maybe String
stripBlockComments "" = Just ""
stripBlockComments ('/' : '*' : rest) = processComment rest
  where
    processComment "" = Nothing
    processComment ('*' : '/' : rs) = stripBlockComments rs
    processComment (_ : rs) = processComment rs
stripBlockComments (c : rest) = case stripBlockComments rest of
  Nothing -> Nothing
  Just rest' -> Just (c : rest')

convertStringsToCharArrays :: String -> Maybe String
convertStringsToCharArrays "" = Just ""
convertStringsToCharArrays ('"' : rest) = case processString rest of
  Nothing -> Nothing
  Just (str, rs) -> case convertStringsToCharArrays rs of
    Nothing -> Nothing
    Just rs' -> Just ('[' : str ++ '0' : ']' : rs')
  where
    processString "" = Nothing
    processString ('"' : rs) = Just ("", rs)
    processString ('\\' : c : rs) = case processString rs of
      Nothing -> Nothing
      Just (str, rs') -> Just ('\'' : '\\' : c : '\'' : ',' : str, rs')
    processString (c : rs) = case processString rs of
      Nothing -> Nothing
      Just (str, rs') -> Just ('\'' : c : '\'' : ',' : str, rs')
convertStringsToCharArrays (c : rest) = case convertStringsToCharArrays rest of
  Nothing -> Nothing
  Just rs -> Just (c : rs)

convertCharsToNumbers :: String -> Maybe String
convertCharsToNumbers "" = Just ""
convertCharsToNumbers ('\'' : rest) = case processChar rest of
  Nothing -> Nothing
  Just (c, rs) -> case convertCharsToNumbers rs of
    Nothing -> Nothing
    Just rs' -> Just (c ++ rs')
  where
    processChar "" = Nothing
    processChar ('\\' : c : '\'' : rs) = case Map.lookup c escapeSequences of
      Just n -> Just (show n, rs)
      Nothing -> Just (show (fromEnum c), rs)
    processChar (c : '\'' : rs) = Just (show (fromEnum c), rs)
    processChar _ = Nothing
convertCharsToNumbers (c : rest) = case convertCharsToNumbers rest of
  Nothing -> Nothing
  Just rs -> Just (c : rs)

arrayToString :: [String] -> Maybe String
arrayToString = traverse processElement . filter (/= ",")
  where
    processElement x = do
      n <- readMaybe x
      if n >= 0 && n < 128
        then Just (chr n)
        else Nothing

removeEscapedLineReturns :: String -> String
removeEscapedLineReturns "" = ""
removeEscapedLineReturns ('\\' : '\r' : '\n' : rest) = removeEscapedLineReturns rest
removeEscapedLineReturns ('\\' : '\n' : rest) = removeEscapedLineReturns rest
removeEscapedLineReturns (c : rest) = c : removeEscapedLineReturns rest

runPreProcessorDirectives :: String -> String -> IO (Maybe String)
runPreProcessorDirectives fileContent filePath =
  runPreProcessorDirectives'
    PPState
      { defines = Map.empty,
        ifStack = [],
        currentFileDir = takeDirectory filePath
      }
    (map tokenize $ lines fileContent)

runPreProcessorDirectives' :: PPState -> [[String]] -> IO (Maybe String)
runPreProcessorDirectives' _ [] = return (Just "")
runPreProcessorDirectives' state ([] : rest) = runPreProcessorDirectives' state rest
runPreProcessorDirectives' state (("#ifdef" : name : restOfLine) : remainingLines) = do
  runPreProcessorDirectives' (state {ifStack = Map.member name (defines state) : ifStack state}) (restOfLine : remainingLines)
runPreProcessorDirectives' state (("#ifndef" : name : restOfLine) : remainingLines) = do
  runPreProcessorDirectives' (state {ifStack = not (Map.member name (defines state)) : ifStack state}) (restOfLine : remainingLines)
runPreProcessorDirectives' state (("#endif" : restOfLine) : remainingLines) = case ifStack state of
  (_ : restOfStack) -> runPreProcessorDirectives' (state {ifStack = restOfStack}) (restOfLine : remainingLines)
  [] -> return Nothing
runPreProcessorDirectives' PPState {defines = defines', ifStack = ifStack', currentFileDir = currentFileDir'} ((_ : restOfLine) : remainingLines)
  | any not ifStack' =
      runPreProcessorDirectives' PPState {defines = defines', ifStack = ifStack', currentFileDir = currentFileDir'} (restOfLine : remainingLines)
runPreProcessorDirectives' state (("#include" : "[" : afterInclude) : remainingLines) =
  case break (== "]") afterInclude of
    (includePath, "]" : restOfLine) ->
      case arrayToString includePath of
        Just includePath' -> do
          file <- tryReadFile includePath' (Just (currentFileDir state))
          case file of
            Right file' ->
              case runFormatter (content file') of
                Just processedContent ->
                  runPreProcessorDirectives'
                    state
                    ( map tokenize (lines processedContent)
                        ++ (restOfLine : remainingLines)
                    )
                Nothing ->
                  return Nothing
            Left _ ->
              return Nothing
        Nothing ->
          return Nothing
    _ ->
      return Nothing
runPreProcessorDirectives' state (("#define" : name : value : restOfLine) : remainingLines) = do
  runPreProcessorDirectives' (state {defines = Map.insert name (Just value) (defines state)}) (restOfLine : remainingLines)
runPreProcessorDirectives' state (("#define" : name : restOfLine) : remainingLines) = do
  runPreProcessorDirectives' (state {defines = Map.insert name Nothing (defines state)}) (restOfLine : remainingLines)
runPreProcessorDirectives' state (("#undef" : name : restOfLine) : remainingLines) = do
  runPreProcessorDirectives' (state {defines = Map.delete name (defines state)}) (restOfLine : remainingLines)
runPreProcessorDirectives' state ((word : restOfLine) : remainingLines) = do
  ( if '#' `elem` word
      then return Nothing
      else
        ( do
            processedRest <- runPreProcessorDirectives' state (restOfLine : remainingLines)
            return $ case processedRest of
              Just processedRestContent' -> case Map.lookup word (defines state) of
                Just (Just value) -> Just $ value ++ ' ' : processedRestContent'
                _ -> Just $ word ++ ' ' : processedRestContent'
              Nothing -> Nothing
        )
    )
