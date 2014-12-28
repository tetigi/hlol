{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HLol.Data.MatchHistory where

import HLol.Data.Match (
    Participant,
    ParticipantIdentity)

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson

data MatchSummary = MatchSummary {
    _mapId :: Int,
    _matchCreation :: Int,
    _matchDuration :: Int,
    _matchId :: Int,
    _matchMode :: String,
    _matchType :: String,
    _matchVersion :: String,
    _participantIdentities :: [ParticipantIdentity],
    _participants :: [Participant],
    _platformId :: String,
    _queueType :: String,
    _region :: String,
    _season :: String
} deriving (Eq, Show)

makeLenses ''MatchSummary

instance FromJSON MatchSummary where
    parseJSON (Object v) = MatchSummary <$>
        v .: "mapId"<*>
        v .: "matchCreation"<*>
        v .: "matchDuration"<*>
        v .: "matchId"<*>
        v .: "matchMode"<*>
        v .: "matchType"<*>
        v .: "matchVersion"<*>
        v .: "participantIdentities"<*>
        v .: "participants"<*>
        v .: "platformId"<*>
        v .: "queueType"<*>
        v .: "region"<*>
        v .: "season"
    parseJSON _ = mzero
data PlayerHistory = PlayerHistory {
    _matches :: [MatchSummary]
} deriving (Eq, Show)

makeLenses ''PlayerHistory

instance FromJSON PlayerHistory where
    parseJSON (Object v) = PlayerHistory <$>
        v .: "matches"
    parseJSON _ = mzero
