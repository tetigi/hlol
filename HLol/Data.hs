{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HLol.Data where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson

data Summoner = Summoner {
    _summonerName :: String,
    _summonerId :: Int,
    _profileIconId :: Int,
    _revisionDate :: Int,
    _summonerLevel :: Int
} deriving (Eq, Show)

makeLenses ''Summoner

instance FromJSON Summoner where
    parseJSON (Object v) =  Summoner <$>
                            v .: "name" <*>
                            v .: "id" <*>
                            v .: "profileIconId" <*>
                            v .: "revisionDate" <*>
                            v .: "summonerLevel"
    parseJSON _          = mzero

data MatchSummary = MatchSummary {
    _matchId :: Int
} deriving (Eq, Show)

makeLenses ''MatchSummary

instance FromJSON MatchSummary where
    parseJSON (Object v)    = MatchSummary <$> v .: "matchId"
    parseJSON _             = mzero

data Player = Player {
    _matchHistoryUri :: String,
    _profileIcon :: Int,
    _playerId :: Int,
    _playerName :: String
} deriving (Show, Eq)

makeLenses ''Player

instance FromJSON Player where
    parseJSON (Object v)    = Player <$>
                                v .: "matchHistoryUri" <*>
                                v .: "profileIcon" <*>
                                v .: "summonerId" <*>
                                v .: "summonerName"
    parseJSON _             = mzero

data ParticipantIdentity = ParticipantIdentity {
    _participantId :: Int,
    _player :: Player
} deriving (Show, Eq)

makeLenses ''ParticipantIdentity

instance FromJSON ParticipantIdentity where
    parseJSON (Object v) = ParticipantIdentity <$>
                            v .: "participantId" <*>
                            v .: "player"
    parseJSON _ = mzero

data MatchDetail = MatchDetail {
    -- TODO More to go here
   _participantIdentities :: [ParticipantIdentity]
} deriving (Show)

makeLenses ''MatchDetail

instance FromJSON MatchDetail where
    parseJSON (Object v)    = MatchDetail <$>
                                v .: "participantIdentities"
    parseJSON _             = mzero
