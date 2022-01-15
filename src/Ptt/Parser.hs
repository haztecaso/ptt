{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Ptt.Parser
  ( ws,
    ws1,
    parseText,
    parseQuotedText,
    parseText',
    parseEmptyToEndOfLine,
    numberGuarded,
    numberBetween,
  )
where

import           Control.Monad    (guard)
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Text.Parsec      ((<?>), (<|>))
import qualified Text.Parsec      as P
import           Text.Parsec.Text (Parser)

ws :: Parser ()
ws = P.skipMany (P.oneOf " \t")

ws1 :: Parser ()
ws1 = P.skipMany1 (P.oneOf " \t")

parseTextSeps :: [Text]
parseTextSeps = map T.pack ["\"", "\r", "\n"]

parseText :: Parser Text
parseText = T.pack <$> P.many (P.noneOf "\r\n")

parseTextNoQuotes :: Parser Text
parseTextNoQuotes = T.pack <$> P.many (P.noneOf "\"\r\n")

parseQuotedText :: Parser Text
parseQuotedText = P.char '\"' *> parseTextNoQuotes <* P.char '\"'

parseText' :: Parser Text
parseText' = P.try parseQuotedText <|> P.try parseText

parseEmptyToEndOfLine :: Parser ()
parseEmptyToEndOfLine = do
  P.skipMany (P.oneOf " \t") `P.endBy1` P.endOfLine
  return ()

numberGuarded :: (Int -> Bool) -> String -> Parser Int
numberGuarded g s = do
  n <- read <$> P.many1 P.digit
  guard (g n) <?> "int " ++ s
  return n

numberBetween :: Int -> Int -> Parser Int
numberBetween a b = numberGuarded (\n -> a <= n && n <= b) $ "between " ++ show a ++ " and " ++ show b
