module Main where

import           Ptt.Journal      (lines2Journal)
import           Ptt.Line
import           Text.Parsec.Text (parseFromFile)

main :: IO ()
main = do
  lines <- parseFromFile parseLines "example.txt"
  case lines of
    Left err -> print err
    Right l -> do
      -- putLines $ map show l
      print $ lines2Journal l
