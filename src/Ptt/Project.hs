{-# LANGUAGE OverloadedStrings #-}

module Ptt.Project
  ( Project,
    pname,
    showProjectName,
    parseProjectName,
    parseProjectSimple,
    parseProject,
    parseProjectDeclaration,
  )
where

import           Data.Text        (Text)
import qualified Data.Text        as T
import           Ptt.Parser
import           Text.Parsec      ((<|>))
import qualified Text.Parsec      as P
import           Text.Parsec.Text (Parser)

data Project = Project
  { pname :: [Text],
    pdesc :: Text
  }

instance Eq Project where
  (==) p1 p2 = pname p1 == pname p2

projectnamesep :: Text
projectnamesep = T.pack ":"

showProjectName :: Project -> String
showProjectName Project {pname = name} = T.unpack (T.intercalate projectnamesep name)

instance Show Project where
  show p = "Project " ++ showProjectName p ++ desc (pdesc p)
    where
      desc "" = ""
      desc d  = ' ' : T.unpack d

parseProjectName :: Parser [Text]
parseProjectName = do
  projectName <- P.many1 (P.letter <|> P.char '-') `P.sepBy1` P.char ':'
  return (map T.pack projectName)

parseProjectDesc :: Parser Text
parseProjectDesc = parseQuotedText <|> parseText

parseProjectSimple :: Parser Project
parseProjectSimple = do
  name <- parseProjectName
  return (Project name "")

parseProject :: Parser Project
parseProject = do
  name <- parseProjectName
  ws
  Project name <$> parseProjectDesc

parseProjectDeclaration :: Parser Project
parseProjectDeclaration = do
  ws
  P.string "Project"
  ws1
  project <- parseProject
  ws
  P.newline
  return project
