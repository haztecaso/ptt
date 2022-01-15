module Ptt.Pomodoro
  ( Pomodoro,
    pwork,
    pomodoroDefault,
  )
where

import           Data.Timeout
import           Ptt.Parser       (numberBetween, ws, ws1)
import qualified Text.Parsec      as P
import           Text.Parsec.Text (Parser)

data Pomodoro = Pomodoro
  { pwork        :: Timeout,
    ppause       :: Timeout,
    plongpause   :: Timeout,
    prepetitions :: Int
  }

pomodoroDefault :: Pomodoro
pomodoroDefault = Pomodoro (25 # Minute) (5 # Minute) (15 # Minute) 4

parsePomodoro :: Parser Pomodoro
parsePomodoro = do
  ws
  P.string "Pomodoro"
  ws1
  work <- numberBetween 1 60
  pause <- numberBetween 1 work
  longpause <- numberBetween 1 work
  repetitions <- numberBetween 2 10
  return $ Pomodoro (fromIntegral work # Minute) (fromIntegral pause # Minute) (fromIntegral longpause # Minute) 4
