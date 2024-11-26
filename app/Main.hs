module Main where

import Basilisk (formatWarning, lint)
import qualified Data.Text as T

main :: IO ()
main = do
  code <- readFile "test.py"
  let warnings = lint (T.pack code)
  mapM_ (putStrLn . T.unpack . formatWarning) warnings