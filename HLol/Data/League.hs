{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HLol.Data.League where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson

data MiniSeriesDto = MiniSeriesDto {
    _losses :: Int,
    _progress :: String,
    _target :: Int,
    _wins :: Int
} deriving (Eq, Show)

makeLenses ''MiniSeriesDto

instance FromJSON MiniSeriesDto where
    parseJSON (Object v) = MiniSeriesDto <$>
        v .: "losses"<*>
        v .: "progress"<*>
        v .: "target"<*>
        v .: "wins"
    parseJSON _ = mzero

data LeagueEntryDto = LeagueEntryDto {
    _division :: String,
    _isFreshBlood :: Bool,
    _isHotStreak :: Bool,
    _isInactive :: Bool,
    _isVeteran :: Bool,
    _leaguePoints :: Int,
    _miniSeries :: MiniSeriesDto,
    _playerOrTeamId :: String,
    _playerOrTeamName :: String,
    _wins :: Int
} deriving (Eq, Show)

makeLenses ''LeagueEntryDto

instance FromJSON LeagueEntryDto where
    parseJSON (Object v) = LeagueEntryDto <$>
        v .: "division"<*>
        v .: "isFreshBlood"<*>
        v .: "isHotStreak"<*>
        v .: "isInactive"<*>
        v .: "isVeteran"<*>
        v .: "leaguePoints"<*>
        v .: "miniSeries"<*>
        v .: "playerOrTeamId"<*>
        v .: "playerOrTeamName"<*>
        v .: "wins"
    parseJSON _ = mzero

data LeagueDto = LeagueDto {
    _entries :: [LeagueEntryDto],
    _name :: String,
    _participantId :: String,
    _queue :: String,
    _tier :: String
} deriving (Eq, Show)

makeLenses ''LeagueDto

instance FromJSON LeagueDto where
    parseJSON (Object v) = LeagueDto <$>
        v .: "entries"<*>
        v .: "name"<*>
        v .: "participantId"<*>
        v .: "queue"<*>
        v .: "tier"
    parseJSON _ = mzero
