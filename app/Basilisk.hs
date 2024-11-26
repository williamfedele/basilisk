module Basilisk where

import Data.Char (isLower, isUpper)
import Data.Text (Text)
import qualified Data.Text as T

data PythonDef
  = Function Text
  | Class Text
  deriving (Show)

data Warning
  = CamelCaseWarning Text
  | SnakeCaseWarning Text
  deriving (Show)

-- Checking classes are in CapitalCamelCase
isCamelCase :: Text -> Bool
isCamelCase name = not (T.null name) && isUpper (head $ T.unpack name)

-- Checking functions are in snake_case
isSnakeCase :: Text -> Bool
isSnakeCase name =
  not (T.null name) && T.all (\c -> isLower c || c == '_' || c `elem` ['0' .. '9']) name && (head (T.unpack name) /= '_')

extractClassName :: Text -> Maybe Text
extractClassName line =
  if T.pack "class " `T.isPrefixOf` T.strip line
    then case T.words (T.takeWhile (/= '(') (T.drop 6 line)) of
      (name : _) -> Just name
      [] -> Nothing
    else Nothing

extractFunctionName :: Text -> Maybe Text
extractFunctionName line =
  if T.pack "def " `T.isPrefixOf` T.strip line
    then case T.words (T.takeWhile (/= '(') (T.drop 4 (T.strip line))) of
      (name : _) ->
        if T.isPrefixOf (T.pack "__") name && T.isSuffixOf (T.pack "__") name
          then Nothing
          else Just name
      [] -> Nothing
    else Nothing

checkName :: PythonDef -> [Warning]
checkName (Class name)
  | not (isCamelCase name) = [CamelCaseWarning name]
  | otherwise = []
checkName (Function name)
  | not (isSnakeCase name) = [SnakeCaseWarning name]
  | otherwise = []

-- Readable formatting
formatWarning :: Warning -> Text
formatWarning (CamelCaseWarning name) =
  T.pack "Class '" <> name <> T.pack "' should be CamelCase"
formatWarning (SnakeCaseWarning name) =
  T.pack "Function '" <> name <> T.pack "' should be snake_case"

lint :: Text -> [Warning]
lint code = concatMap processLine (T.lines code)
  where
    processLine :: Text -> [Warning]
    processLine line
      | Just name <- extractClassName line = checkName (Class name)
      | Just name <- extractFunctionName line = checkName (Function name)
      | otherwise = []
