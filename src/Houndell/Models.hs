{-# LANGUAGE OverloadedStrings #-}

module Houndell.Models where

import           Data.Aeson     (FromJSON (..), Value (..), (.:))
import           Data.Map
import           Data.Text.Lazy (Text (..))

data Match = Match {
    after      :: [Text],
    before     :: [Text],
    line       :: Text,
    lineNumber :: Int
} deriving (Show)

instance FromJSON Match where
    parseJSON (Object v) =
        Match <$>
        v .: "After" <*>
        v .: "Before" <*>
        v .: "Line" <*>
        v .: "LineNumber"


data FileMatches = FileMatches {
    filename :: Text,
    matches  :: [Match]

} deriving (Show)

instance FromJSON FileMatches where
    parseJSON (Object v) =
        FileMatches <$>
        v .: "Filename" <*>
        v .: "Matches"


data RepoMatches = RepoMatches {
    filesWithMatch :: Int,
    files          :: [FileMatches],
    revision       :: Text
} deriving (Show)

instance FromJSON RepoMatches where
    parseJSON (Object v) =
        RepoMatches <$>
        v .: "FilesWithMatch" <*>
        v .: "Matches" <*>
        v .: "Revision"


data HoundResultsStats = HoundResultsStats {
    duration    :: Int,
    filesOpened :: Int
} deriving (Show)

instance FromJSON HoundResultsStats where
    parseJSON (Object v) =
        HoundResultsStats <$>
        v .: "Duration" <*>
        v .: "FilesOpened"


type HoundResults = Map Text RepoMatches
data HoundResponse = HoundResponse {
    results :: HoundResults,
    stats   :: HoundResultsStats
} deriving (Show)

instance FromJSON HoundResponse where
    parseJSON (Object v) =
        HoundResponse <$>
        v .: "Results" <*>
        v .: "Stats"
