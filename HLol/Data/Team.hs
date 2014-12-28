{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HLol.Data.Summoner where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson

data TeamMemberInfoDto = TeamMemberInfoDto {
    _inviteDate :: Int,
    _joinDate :: Int,
    _playerId :: Int,
    _status :: String
} deriving (Eq, Show)

makeLenses ''TeamMemberInfoDto

instance FromJSON TeamMemberInfoDto where
    parseJSON (Object v) = TeamMemberInfoDto <$>
        v .: "inviteDate"<*>
        v .: "joinDate"<*>
        v .: "playerId"<*>
        v .: "status"
    parseJSON _ = mzero
data TeamStatDetailDto = TeamStatDetailDto {
    _averageGamesPlayed :: Int,
    _losses :: Int,
    _teamStatType :: String,
    _wins :: Int
} deriving (Eq, Show)

makeLenses ''TeamStatDetailDto

instance FromJSON TeamStatDetailDto where
    parseJSON (Object v) = TeamStatDetailDto <$>
        v .: "averageGamesPlayed"<*>
        v .: "losses"<*>
        v .: "teamStatType"<*>
        v .: "wins"
    parseJSON _ = mzero
data RosterDto = RosterDto {
    _memberList :: [TeamMemberInfoDto],
    _ownerId :: Int
} deriving (Eq, Show)

makeLenses ''RosterDto

instance FromJSON RosterDto where
    parseJSON (Object v) = RosterDto <$>
        v .: "memberList"<*>
        v .: "ownerId"
    parseJSON _ = mzero
data MatchHistorySummaryDto = MatchHistorySummaryDto {
    _assists :: Int,
    _date :: Int,
    _deaths :: Int,
    _gameId :: Int,
    _gameMode :: String,
    _invalid :: Bool,
    _kills :: Int,
    _mapId :: Int,
    _opposingTeamKills :: Int,
    _opposingTeamName :: String,
    _win :: Bool
} deriving (Eq, Show)

makeLenses ''MatchHistorySummaryDto

instance FromJSON MatchHistorySummaryDto where
    parseJSON (Object v) = MatchHistorySummaryDto <$>
        v .: "assists"<*>
        v .: "date"<*>
        v .: "deaths"<*>
        v .: "gameId"<*>
        v .: "gameMode"<*>
        v .: "invalid"<*>
        v .: "kills"<*>
        v .: "mapId"<*>
        v .: "opposingTeamKills"<*>
        v .: "opposingTeamName"<*>
        v .: "win"
    parseJSON _ = mzero
data TeamDto = TeamDto {
    _createDate :: Int,
    _fullId :: String,
    _lastGameDate :: Int,
    _lastJoinDate :: Int,
    _lastJoinedRankedTeamQueueDate :: Int,
    _matchHistory :: [MatchHistorySummaryDto],
    _modifyDate :: Int,
    _name :: String,
    _roster :: RosterDto,
    _secondLastJoinDate :: Int,
    _status :: String,
    _tag :: String,
    _teamStatDetails :: [TeamStatDetailDto],
    _thirdLastJoinDate :: Int
} deriving (Eq, Show)

makeLenses ''TeamDto

instance FromJSON TeamDto where
    parseJSON (Object v) = TeamDto <$>
        v .: "createDate"<*>
        v .: "fullId"<*>
        v .: "lastGameDate"<*>
        v .: "lastJoinDate"<*>
        v .: "lastJoinedRankedTeamQueueDate"<*>
        v .: "matchHistory"<*>
        v .: "modifyDate"<*>
        v .: "name"<*>
        v .: "roster"<*>
        v .: "secondLastJoinDate"<*>
        v .: "status"<*>
        v .: "tag"<*>
        v .: "teamStatDetails"<*>
        v .: "thirdLastJoinDate"
    parseJSON _ = mzero
