{-# LANGUAGE OverloadedStrings #-}

module Ptt.Entry
  ( Entry,
    EntryGroup,
    parseEntry,
    parseEntryGroup,
    entryTotalTime,
    entryGroupTotalTime,
    showTimeout,
  )
where

import           Control.Monad                 (guard, replicateM)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time.Calendar            (Day, fromGregorian)
import           Data.Timeout
import           Ptt.Parser
import           Ptt.Pomodoro                  (Pomodoro (pwork),
                                                pomodoroDefault)
import           Ptt.Project
import           Text.Parsec                   ((<?>))
import qualified Text.Parsec                   as P
import           Text.Parsec.Text              (Parser)
import           Text.ParserCombinators.Parsec ((<|>))
import           Text.Printf                   (printf)

data Entry
  = EntryD
      { eproject  :: Project,
        eduration :: Timeout
      }
  | EntryP
      { eproject   :: Project,
        epconfig   :: Pomodoro,
        ensessions :: Int
      }

entryTotalTime :: Entry -> Timeout
entryTotalTime EntryD {eduration = d} = d
entryTotalTime EntryP {epconfig = pc, ensessions = n} = sum (replicate n (pwork pc))

showEntryWithPadding :: Int -> Entry -> String
showEntryWithPadding pd EntryD {eproject = p, eduration = d} =
  showProjectName p ++ replicate pd ' ' ++ showTimeout d
showEntryWithPadding pd e@EntryP {eproject = p, epconfig = pc, ensessions = n} =
  showProjectName p ++ replicate pd ' ' ++ dots ++ " # " ++ showTimeout total
  where
    total = entryTotalTime e
    dots = insert 4 ' ' (replicate n '.')

insert :: Int -> a -> [a] -> [a]
insert 0 y xs = xs
insert n y [] = []
insert n y xs
  | length xs < n = xs
  | otherwise = take n xs ++ [y] ++ insert n y (drop n xs)

instance Show Entry where
  show = showEntryWithPadding 1

data EntryGroup = EntryGroup
  { egdate    :: Day,
    egdesc    :: Text,
    egentries :: [Entry],
    egpconfig :: Pomodoro
  }

entryGroupTotalTime :: EntryGroup -> Timeout
entryGroupTotalTime EntryGroup {egentries = entries} = sum $ map entryTotalTime entries

showEntryGroupWithPadding :: Int -> EntryGroup -> String
showEntryGroupWithPadding pd g =
  show (egdate g) ++ " " ++ T.unpack (egdesc g) ++ "\n"
    ++ unlines (map (("  " ++) . showP) entries)
  where
    entries = egentries g
    len e = length $ showProjectName $ eproject e
    maxlen = pd + maximum (map len entries)
    showP e = showEntryWithPadding (maxlen - len e) e

instance Show EntryGroup where
  show = showEntryGroupWithPadding 2

timesep = ':'

timeoutHMS :: Timeout -> (Int, Int, Int)
timeoutHMS (Timeout n) = (hours, minutes, seconds)
  where
    seconds = fromEnum $ n `div` 1000000000 `mod` 60
    minutes = fromEnum $ n `div` 1000000000 `div` 60 `mod` 60
    hours = fromEnum $ n `div` 1000000000 `div` 3600

print2digits = printf "%02v"

showTimeoutHMS :: Timeout -> String
showTimeoutHMS t = h ++ [timesep] ++ m ++ [timesep] ++ s
  where
    (hours, minutes, seconds) = timeoutHMS t
    s = print2digits seconds
    m = print2digits minutes
    h = print2digits hours

showTimeoutHM :: Timeout -> String
showTimeoutHM t = h ++ [timesep] ++ m
  where
    (hours, minutes, seconds) = timeoutHMS t
    m = print2digits minutes
    h = print2digits hours

showTimeoutM :: Timeout -> String
showTimeoutM t = m
  where
    (_, minutes, _) = timeoutHMS t
    m = print2digits minutes

showTimeout :: Timeout -> String
showTimeout t =
  if seconds == 0
    then
      ( if hours == 0
          then showTimeoutM t
          else showTimeoutHM t
      )
    else showTimeoutHMS t
  where
    (hours, _, seconds) = timeoutHMS t

daysep :: Char
daysep = '-'

yearMonthDayToString :: Integer -> Int -> Int -> String
yearMonthDayToString y m d = show y ++ [daysep] ++ print2digits m ++ [daysep] ++ print2digits d

parseDay :: Parser Day
parseDay = do
  year <- read <$> replicateM 4 P.digit
  P.char daysep
  month <- read <$> replicateM 2 P.digit
  guard (month > 0 && month <= 12) <?> "valid month (1–12)"
  P.char daysep
  day <- read <$> replicateM 2 P.digit
  guard (day > 0 && day <= 31) <?> "valid day (1–31)"
  guard (show (fromGregorian year month day) == yearMonthDayToString year month day) <?> "valid gregorian date"
  return $ fromGregorian year month day

parseTimeoutHM :: Parser Timeout
parseTimeoutHM = do
  h <- fromIntegral <$> numberBetween 0 23
  P.char timesep
  m <- fromIntegral <$> numberBetween 0 59
  return $ h # Hour + m # Minute

parseTimeoutHMS :: Parser Timeout
parseTimeoutHMS = do
  t <- parseTimeoutHM
  P.char timesep
  s <- fromIntegral <$> numberBetween 0 59
  return $ t + s # Second

parseTimeout :: Parser Timeout
parseTimeout = P.try parseTimeoutHMS <|> P.try parseTimeoutHM

parseEntryD :: Pomodoro -> Parser Entry
parseEntryD _ = do
  ws1
  project <- parseProjectSimple
  ws1
  timeout <- parseTimeout
  ws
  P.newline
  return $ EntryD project timeout

parseEntryP :: Pomodoro -> Parser Entry
parseEntryP p = do
  ws1
  project <- parseProjectSimple
  ws1
  dots <- P.many1 (P.char '.' <* ws)
  ws
  P.newline
  return $ EntryP project p (length dots)

parseEntry :: Pomodoro -> Parser Entry
parseEntry p = P.try (parseEntryD p) <|> P.try (parseEntryP p)

parseEntryGroupHeader :: Parser EntryGroup
parseEntryGroupHeader = do
  day <- parseDay
  ws1
  desc <- parseText'
  ws
  P.newline
  return (EntryGroup day desc [] pomodoroDefault)

parseEntryGroup :: Parser EntryGroup
parseEntryGroup = do
  group <- parseEntryGroupHeader
  entries <- P.many1 (parseEntry (egpconfig group))
  return $ group {egentries = entries}
