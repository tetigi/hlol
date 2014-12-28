{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HLol.Data.Match where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.Map as M

data Position = Position {
    _x :: Int,
    _y :: Int
} deriving (Eq, Show)

makeLenses ''Position

instance FromJSON Position where
    parseJSON (Object v) = Position <$>
        v .: "x"<*>
        v .: "y"
    parseJSON _ = mzero
data ParticipantFrame = ParticipantFrame {
    _currentGold :: Int,
    _jungleMinionsKilled :: Int,
    _level :: Int,
    _participantMinionsKilled :: Int,
    _participantFrameId :: Int,
    _participantFramePosition :: Position,
    _totalGold :: Int,
    _xp :: Int
} deriving (Eq, Show)

makeLenses ''ParticipantFrame

instance FromJSON ParticipantFrame where
    parseJSON (Object v) = ParticipantFrame <$>
        v .: "currentGold"<*>
        v .: "jungleMinionsKilled"<*>
        v .: "level"<*>
        v .: "minionsKilled"<*>
        v .: "participantId"<*>
        v .: "position"<*>
        v .: "totalGold"<*>
        v .: "xp"
    parseJSON _ = mzero
data Event = Event {
    _ascendedType :: String,
    _assistingParticipantIds :: [Int],
    _buildingType :: String,
    _creatorId :: Int,
    _eventType :: String,
    _itemAfter :: Int,
    _itemBefore :: Int,
    _itemId :: Int,
    _killerId :: Int,
    _laneType :: String,
    _levelUpType :: String,
    _monsterType :: String,
    _eventParticipantId :: Int,
    _pointCaptured :: String,
    _position :: Position,
    _skillSlot :: Int,
    _eventTeamId :: Int,
    _eventTimestamp :: Int,
    _towerType :: String,
    _victimId :: Int,
    _wardType :: String
} deriving (Eq, Show)

makeLenses ''Event

instance FromJSON Event where
    parseJSON (Object v) = Event <$>
        v .: "ascendedType"<*>
        v .: "assistingParticipantIds"<*>
        v .: "buildingType"<*>
        v .: "creatorId"<*>
        v .: "eventType"<*>
        v .: "itemAfter"<*>
        v .: "itemBefore"<*>
        v .: "itemId"<*>
        v .: "killerId"<*>
        v .: "laneType"<*>
        v .: "levelUpType"<*>
        v .: "monsterType"<*>
        v .: "participantId"<*>
        v .: "pointCaptured"<*>
        v .: "position"<*>
        v .: "skillSlot"<*>
        v .: "teamId"<*>
        v .: "timestamp"<*>
        v .: "towerType"<*>
        v .: "victimId"<*>
        v .: "wardType"
    parseJSON _ = mzero
data ParticipantTimelineData = ParticipantTimelineData {
    _tenToTwenty :: Double,
    _thirtyToEnd :: Double,
    _twentyToThirty :: Double,
    _zeroToTen :: Double
} deriving (Eq, Show)

makeLenses ''ParticipantTimelineData

instance FromJSON ParticipantTimelineData where
    parseJSON (Object v) = ParticipantTimelineData <$>
        v .: "tenToTwenty"<*>
        v .: "thirtyToEnd"<*>
        v .: "twentyToThirty"<*>
        v .: "zeroToTen"
    parseJSON _ = mzero
data Frame = Frame {
    _events :: [Event],
    _participantFrames :: M.Map String ParticipantFrame,
    _timestamp :: Int
} deriving (Eq, Show)

makeLenses ''Frame

instance FromJSON Frame where
    parseJSON (Object v) = Frame <$>
        v .: "events"<*>
        v .: "participantFrames"<*>
        v .: "timestamp"
    parseJSON _ = mzero
data BannedChampion = BannedChampion {
    _champId :: Int,
    _pickTurn :: Int
} deriving (Eq, Show)

makeLenses ''BannedChampion

instance FromJSON BannedChampion where
    parseJSON (Object v) = BannedChampion <$>
        v .: "championId"<*>
        v .: "pickTurn"
    parseJSON _ = mzero
data Player = Player {
    _matchHistoryUri :: String,
    _profileIcon :: Int,
    _summonerId :: Int,
    _summonerName :: String
} deriving (Eq, Show)

makeLenses ''Player

instance FromJSON Player where
    parseJSON (Object v) = Player <$>
        v .: "matchHistoryUri"<*>
        v .: "profileIcon"<*>
        v .: "summonerId"<*>
        v .: "summonerName"
    parseJSON _ = mzero
data Rune = Rune {
    _runeRank :: Int,
    _runeId :: Int
} deriving (Eq, Show)

makeLenses ''Rune

instance FromJSON Rune where
    parseJSON (Object v) = Rune <$>
        v .: "rank"<*>
        v .: "runeId"
    parseJSON _ = mzero
data ParticipantTimeline = ParticipantTimeline {
    _ancientGolemAssistsPerMinCounts :: ParticipantTimelineData,
    _ancientGolemKillsPerMinCounts :: ParticipantTimelineData,
    _assistedLaneDeathsPerMinDeltas :: ParticipantTimelineData,
    _assistedLaneKillsPerMinDeltas :: ParticipantTimelineData,
    _baronAssistsPerMinCounts :: ParticipantTimelineData,
    _baronKillsPerMinCounts :: ParticipantTimelineData,
    _creepsPerMinDeltas :: ParticipantTimelineData,
    _csDiffPerMinDeltas :: ParticipantTimelineData,
    _damageTakenDiffPerMinDeltas :: ParticipantTimelineData,
    _damageTakenPerMinDeltas :: ParticipantTimelineData,
    _dragonAssistsPerMinCounts :: ParticipantTimelineData,
    _dragonKillsPerMinCounts :: ParticipantTimelineData,
    _elderLizardAssistsPerMinCounts :: ParticipantTimelineData,
    _elderLizardKillsPerMinCounts :: ParticipantTimelineData,
    _goldPerMinDeltas :: ParticipantTimelineData,
    _inhibitorAssistsPerMinCounts :: ParticipantTimelineData,
    _inhibitorKillsPerMinCounts :: ParticipantTimelineData,
    _lane :: String,
    _role :: String,
    _towerAssistsPerMinCounts :: ParticipantTimelineData,
    _towerKillsPerMinCounts :: ParticipantTimelineData,
    _towerKillsPerMinDeltas :: ParticipantTimelineData,
    _vilemawAssistsPerMinCounts :: ParticipantTimelineData,
    _vilemawKillsPerMinCounts :: ParticipantTimelineData,
    _wardsPerMinDeltas :: ParticipantTimelineData,
    _xpDiffPerMinDeltas :: ParticipantTimelineData,
    _xpPerMinDeltas :: ParticipantTimelineData
} deriving (Eq, Show)

makeLenses ''ParticipantTimeline

instance FromJSON ParticipantTimeline where
    parseJSON (Object v) = ParticipantTimeline <$>
        v .: "ancientGolemAssistsPerMinCounts"<*>
        v .: "ancientGolemKillsPerMinCounts"<*>
        v .: "assistedLaneDeathsPerMinDeltas"<*>
        v .: "assistedLaneKillsPerMinDeltas"<*>
        v .: "baronAssistsPerMinCounts"<*>
        v .: "baronKillsPerMinCounts"<*>
        v .: "creepsPerMinDeltas"<*>
        v .: "csDiffPerMinDeltas"<*>
        v .: "damageTakenDiffPerMinDeltas"<*>
        v .: "damageTakenPerMinDeltas"<*>
        v .: "dragonAssistsPerMinCounts"<*>
        v .: "dragonKillsPerMinCounts"<*>
        v .: "elderLizardAssistsPerMinCounts"<*>
        v .: "elderLizardKillsPerMinCounts"<*>
        v .: "goldPerMinDeltas"<*>
        v .: "inhibitorAssistsPerMinCounts"<*>
        v .: "inhibitorKillsPerMinCounts"<*>
        v .: "lane"<*>
        v .: "role"<*>
        v .: "towerAssistsPerMinCounts"<*>
        v .: "towerKillsPerMinCounts"<*>
        v .: "towerKillsPerMinDeltas"<*>
        v .: "vilemawAssistsPerMinCounts"<*>
        v .: "vilemawKillsPerMinCounts"<*>
        v .: "wardsPerMinDeltas"<*>
        v .: "xpDiffPerMinDeltas"<*>
        v .: "xpPerMinDeltas"
    parseJSON _ = mzero
data ParticipantStats = ParticipantStats {
    _assists :: Int,
    _champLevel :: Int,
    _combatPlayerScore :: Int,
    _deaths :: Int,
    _doubleKills :: Int,
    _firstBloodAssist :: Bool,
    _firstBloodKill :: Bool,
    _firstInhibitorAssist :: Bool,
    _firstInhibitorKill :: Bool,
    _firstTowerAssist :: Bool,
    _firstTowerKill :: Bool,
    _goldEarned :: Int,
    _goldSpent :: Int,
    _participantInhibitorKills :: Int,
    _item0 :: Int,
    _item1 :: Int,
    _item2 :: Int,
    _item3 :: Int,
    _item4 :: Int,
    _item5 :: Int,
    _item6 :: Int,
    _killingSprees :: Int,
    _kills :: Int,
    _largestCriticalStrike :: Int,
    _largestKillingSpree :: Int,
    _largestMultiKill :: Int,
    _magicDamageDealt :: Int,
    _magicDamageDealtToChampions :: Int,
    _magicDamageTaken :: Int,
    _minionsKilled :: Int,
    _neutralMinionsKilled :: Int,
    _neutralMinionsKilledEnemyJungle :: Int,
    _neutralMinionsKilledTeamJungle :: Int,
    _nodeCapture :: Int,
    _nodeCaptureAssist :: Int,
    _nodeNeutralize :: Int,
    _nodeNeutralizeAssist :: Int,
    _objectivePlayerScore :: Int,
    _pentaKills :: Int,
    _physicalDamageDealt :: Int,
    _physicalDamageDealtToChampions :: Int,
    _physicalDamageTaken :: Int,
    _quadraKills :: Int,
    _sightWardsBoughtInGame :: Int,
    _teamObjective :: Int,
    _totalDamageDealt :: Int,
    _totalDamageDealtToChampions :: Int,
    _totalDamageTaken :: Int,
    _totalHeal :: Int,
    _totalPlayerScore :: Int,
    _totalScoreRank :: Int,
    _totalTimeCrowdControlDealt :: Int,
    _totalUnitsHealed :: Int,
    _participantTowerKills :: Int,
    _tripleKills :: Int,
    _trueDamageDealt :: Int,
    _trueDamageDealtToChampions :: Int,
    _trueDamageTaken :: Int,
    _unrealKills :: Int,
    _visionWardsBoughtInGame :: Int,
    _wardsKilled :: Int,
    _wardsPlaced :: Int,
    _participantWinner :: Bool
} deriving (Eq, Show)

makeLenses ''ParticipantStats

instance FromJSON ParticipantStats where
    parseJSON (Object v) = ParticipantStats <$>
        v .: "assists"<*>
        v .: "champLevel"<*>
        v .: "combatPlayerScore"<*>
        v .: "deaths"<*>
        v .: "doubleKills"<*>
        v .: "firstBloodAssist"<*>
        v .: "firstBloodKill"<*>
        v .: "firstInhibitorAssist"<*>
        v .: "firstInhibitorKill"<*>
        v .: "firstTowerAssist"<*>
        v .: "firstTowerKill"<*>
        v .: "goldEarned"<*>
        v .: "goldSpent"<*>
        v .: "inhibitorKills"<*>
        v .: "item0"<*>
        v .: "item1"<*>
        v .: "item2"<*>
        v .: "item3"<*>
        v .: "item4"<*>
        v .: "item5"<*>
        v .: "item6"<*>
        v .: "killingSprees"<*>
        v .: "kills"<*>
        v .: "largestCriticalStrike"<*>
        v .: "largestKillingSpree"<*>
        v .: "largestMultiKill"<*>
        v .: "magicDamageDealt"<*>
        v .: "magicDamageDealtToChampions"<*>
        v .: "magicDamageTaken"<*>
        v .: "minionsKilled"<*>
        v .: "neutralMinionsKilled"<*>
        v .: "neutralMinionsKilledEnemyJungle"<*>
        v .: "neutralMinionsKilledTeamJungle"<*>
        v .: "nodeCapture"<*>
        v .: "nodeCaptureAssist"<*>
        v .: "nodeNeutralize"<*>
        v .: "nodeNeutralizeAssist"<*>
        v .: "objectivePlayerScore"<*>
        v .: "pentaKills"<*>
        v .: "physicalDamageDealt"<*>
        v .: "physicalDamageDealtToChampions"<*>
        v .: "physicalDamageTaken"<*>
        v .: "quadraKills"<*>
        v .: "sightWardsBoughtInGame"<*>
        v .: "teamObjective"<*>
        v .: "totalDamageDealt"<*>
        v .: "totalDamageDealtToChampions"<*>
        v .: "totalDamageTaken"<*>
        v .: "totalHeal"<*>
        v .: "totalPlayerScore"<*>
        v .: "totalScoreRank"<*>
        v .: "totalTimeCrowdControlDealt"<*>
        v .: "totalUnitsHealed"<*>
        v .: "towerKills"<*>
        v .: "tripleKills"<*>
        v .: "trueDamageDealt"<*>
        v .: "trueDamageDealtToChampions"<*>
        v .: "trueDamageTaken"<*>
        v .: "unrealKills"<*>
        v .: "visionWardsBoughtInGame"<*>
        v .: "wardsKilled"<*>
        v .: "wardsPlaced"<*>
        v .: "winner"
    parseJSON _ = mzero
data Mastery = Mastery {
    _masteryId :: Int,
    _masteryRank :: Int
} deriving (Eq, Show)

makeLenses ''Mastery

instance FromJSON Mastery where
    parseJSON (Object v) = Mastery <$>
        v .: "masteryId"<*>
        v .: "rank"
    parseJSON _ = mzero
data Timeline = Timeline {
    _frameInterval :: Int,
    _frames :: [Frame]
} deriving (Eq, Show)

makeLenses ''Timeline

instance FromJSON Timeline where
    parseJSON (Object v) = Timeline <$>
        v .: "frameInterval"<*>
        v .: "frames"
    parseJSON _ = mzero
data Team = Team {
    _bans :: [BannedChampion],
    _baronKills :: Int,
    _dominionVictoryScore :: Int,
    _dragonKills :: Int,
    _firstBaron :: Bool,
    _firstBlood :: Bool,
    _firstDragon :: Bool,
    _firstInhibitor :: Bool,
    _firstTower :: Bool,
    _inhibitorKills :: Int,
    _teamId :: Int,
    _towerKills :: Int,
    _vilemawKills :: Int,
    _winner :: Bool
} deriving (Eq, Show)

makeLenses ''Team

instance FromJSON Team where
    parseJSON (Object v) = Team <$>
        v .: "bans"<*>
        v .: "baronKills"<*>
        v .: "dominionVictoryScore"<*>
        v .: "dragonKills"<*>
        v .: "firstBaron"<*>
        v .: "firstBlood"<*>
        v .: "firstDragon"<*>
        v .: "firstInhibitor"<*>
        v .: "firstTower"<*>
        v .: "inhibitorKills"<*>
        v .: "teamId"<*>
        v .: "towerKills"<*>
        v .: "vilemawKills"<*>
        v .: "winner"
    parseJSON _ = mzero
data ParticipantIdentity = ParticipantIdentity {
    _pId :: Int,
    _player :: Player
} deriving (Eq, Show)

makeLenses ''ParticipantIdentity

instance FromJSON ParticipantIdentity where
    parseJSON (Object v) = ParticipantIdentity <$>
        v .: "participantId"<*>
        v .: "player"
    parseJSON _ = mzero
data Participant = Participant {
    _championId :: Int,
    _highestAchievedSeasonTier :: String,
    _masteries :: [Mastery],
    _participantId :: Int,
    _runes :: [Rune],
    _spell1Id :: Int,
    _spell2Id :: Int,
    _stats :: ParticipantStats,
    _participantTeamId :: Int,
    _participantTimeline :: ParticipantTimeline
} deriving (Eq, Show)

makeLenses ''Participant

instance FromJSON Participant where
    parseJSON (Object v) = Participant <$>
        v .: "championId"<*>
        v .: "highestAchievedSeasonTier"<*>
        v .: "masteries"<*>
        v .: "participantId"<*>
        v .: "runes"<*>
        v .: "spell1Id"<*>
        v .: "spell2Id"<*>
        v .: "stats"<*>
        v .: "teamId"<*>
        v .: "timeline"
    parseJSON _ = mzero
data MatchDetail = MatchDetail {
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
    _season :: String,
    _teams :: [Team],
    _timeline :: Timeline
} deriving (Eq, Show)

makeLenses ''MatchDetail

instance FromJSON MatchDetail where
    parseJSON (Object v) = MatchDetail <$>
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
        v .: "season"<*>
        v .: "teams"<*>
        v .: "timeline"
    parseJSON _ = mzero
