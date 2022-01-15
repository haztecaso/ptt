{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Ptt.Journal where

import           Data.Text    (Text)
import           Data.Timeout
import           Ptt.Entry    (EntryGroup, entryGroupTotalTime, showTimeout)
import           Ptt.Line
import           Ptt.Project  (Project)

-- | A Journal, containing entries and various other things.
-- The basic data model for ptt.
data Journal = Journal
  { jegroups  :: [EntryGroup],
    jprojects :: [Project]
    -- jfiles   :: [(FilePath, Text)]
  }

instance Show Journal where
  show j =
    "Journal with " ++ show nprojects ++ " projects and " ++ show negroups ++ " entry groups.\n"
      ++ "Total time: "
      ++ showTimeout (journalTotalTime j)
    where
      nprojects = length $ jprojects j
      negroups = length $ jegroups j

journalTotalTime :: Journal -> Timeout
journalTotalTime Journal {jegroups = groups} = sum $ map entryGroupTotalTime groups

lines2Journal :: [Line] -> Journal
lines2Journal [] = Journal [] []
lines2Journal (LineProjectDeclaration p : lines) = journal {jprojects = projects ++ [p]}
  where
    journal = lines2Journal lines
    projects = jprojects journal
lines2Journal (LineEntryGroup eg : lines) = journal {jegroups = egroups ++ [eg]}
  where
    journal = lines2Journal lines
    egroups = jegroups journal
lines2Journal (_ : lines) = lines2Journal lines
