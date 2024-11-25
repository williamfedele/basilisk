module Basilisk where

import Data.Char (isUpper)
import Data.Text (Text)
import qualified Data.Text as T

data PythonDef
  = Class Text
  deriving (Show)

data Warning
  = CamelCaseWarning Text
  deriving (Show)

-- Checking classes are in CamelCase
isCamelCase :: Text -> Bool
isCamelCase name = not (T.null name) && isUpper (head $ T.unpack name)

extractClassName :: Text -> Maybe Text
extractClassName line =
  if T.pack "class " `T.isPrefixOf` line
    then case T.words (T.takeWhile (/= '(') (T.drop 6 line)) of
      (name : _) -> Just name
      [] -> Nothing
    else Nothing

checkName :: PythonDef -> [Warning]
checkName (Class name)
  | not (isCamelCase name) = [CamelCaseWarning name]
  | otherwise = []

-- Readable formatting
formatWarning :: Warning -> Text
formatWarning (CamelCaseWarning name) =
  T.pack "Class '" <> name <> T.pack "' should be CamelCase"

lint :: Text -> [Warning]
lint code =
  let lines' = zip [1 ..] (T.lines code)
      lineChecks =
        concatMap
          ( \(n, line) ->
              case extractClassName line of
                Just name -> checkName (Class name)
                Nothing -> []
          )
          lines'
   in lineChecks
