module Main where

import Basilisk (formatWarning, lint)
import Data.Text (Text, unpack)
import qualified Data.Text as T

main :: IO ()
main = do
  code <- readFile "test.py"
  let warnings = lint (T.pack code)
  mapM_ (putStrLn . T.unpack . formatWarning) warnings