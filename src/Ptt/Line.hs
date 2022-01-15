{-# LANGUAGE OverloadedStrings #-}

module Ptt.Line
  ( Line
      ( LineEmpty,
        LineComment,
        LineProjectDeclaration,
        LineEntryGroup,
        LineText
      ),
    parseLine,
    parseLines,
    putLines,
  )
where

import           Data.Text        (Text)
import qualified Data.Text        as T
import           Ptt.Entry        (EntryGroup, parseEntryGroup)
import           Ptt.Parser
import           Ptt.Project      (Project, parseProjectDeclaration)
import           Text.Parsec      (eof)
import qualified Text.Parsec      as P
import           Text.Parsec.Text (Parser)

data Line
  = LineEmpty
  | LineComment Text
  | LineProjectDeclaration Project
  | LineEntryGroup EntryGroup
  | LineText Text

instance Show Line where
  show LineEmpty                  = ""
  show (LineComment t)            = T.unpack t
  show (LineProjectDeclaration p) = show p
  show (LineEntryGroup e)         = show e
  show (LineText t)               = "NOT PARSED: " ++ T.unpack t

singleton :: a -> [a]
singleton a = [a]

parseLineEmpty :: Parser Line
parseLineEmpty = P.try $ do
  ws
  P.newline
  return LineEmpty

parseLineComment :: Parser Line
parseLineComment =
  P.try $
    LineComment <$> do
      ws
      P.char '#'
      text <- parseText
      ws
      P.newline
      return $ T.append (T.pack "#") text

parseLineText :: Parser Line
parseLineText =
  P.try $
    LineText <$> do
      text <- parseText
      P.newline
      return text

parseLine :: Parser Line
parseLine =
  P.choice
    [ parseLineEmpty,
      parseLineComment,
      P.try $ LineEntryGroup <$> parseEntryGroup,
      P.try $ LineProjectDeclaration <$> parseProjectDeclaration
      -- parseLineText
    ]

parseLines :: Parser [Line]
parseLines = do
  line <- parseLine
  lines <- P.many $ do
    parseLine
  eof
  return (line : lines)

putLines :: [String] -> IO ()
putLines [] = return ()
putLines (s : ss) = do
  putStrLn s
  putLines ss
