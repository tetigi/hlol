{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HLol.Data.Game where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson

data PlayerDto = PlayerDto {
    _playerChampionId :: Int,
    _playerSummonerId :: Int,
    _playerTeamId :: Int
} deriving (Show, Eq)

makeLenses ''PlayerDto

instance FromJSON PlayerDto where
    parseJSON (Object v) = PlayerDto <$>
        v .: "championId" <*>
        v .: "summonerId" <*>
        v .: "teamId"

    parseJSON _ = mzero

data RawStatsDto = RawStatsDto {
    _assists :: Int,
    _barracksKilled :: Int,
    _championsKilled :: Int,
    _combatPlayerScore :: Int,
    _consumablesPurchased :: Int,
    _damageDealtPlayer :: Int,
    _doubleKills :: Int,
    _firstBlood :: Int,
    _gold :: Int,
    _goldEarned :: Int,
    _goldSpent :: Int,
    _item0 :: Int,
    _item1 :: Int,
    _item2 :: Int,
    _item3 :: Int,
    _item4 :: Int,
    _item5 :: Int,
    _item6 :: Int,
    _itemsPurchased :: Int,
    _killingSprees :: Int,
    _largestCriticalStrike :: Int,
    _largestKillingSpree :: Int,
    _largestMultiKill :: Int,
    _legendaryItemsCreated :: Int,
    _level :: Int,
    _magicDamageDealtPlayer :: Int,
    _magicDamageDealtToChampions :: Int,
    _magicDamageTaken :: Int,
    _minionsDenied :: Int,
    _minionsKilled :: Int,
    _neutralMinionsKilled :: Int,
    _neutralMinionsKilledEnemyJungle :: Int,
    _neutralMinionsKilledYourJungle :: Int,
    _nexusKilled :: Bool,
    _nodeCapture :: Int,
    _nodeCaptureAssist :: Int,
    _nodeNeutralize :: Int,
    _nodeNeutralizeAssist :: Int,
    _numDeaths :: Int,
    _numItemsBought :: Int,
    _objectivePlayerScore :: Int,
    _pentaKills :: Int,
    _physicalDamageDealtPlayer :: Int,
    _physicalDamageDealtToChampions :: Int,
    _physicalDamageTaken :: Int,
    _quadraKills :: Int,
    _sightWardsBought :: Int,
    _spell1Cast :: Int,
    _spell2Cast :: Int,
    _spell3Cast :: Int,
    _spell4Cast :: Int,
    _summonSpell1Cast :: Int,
    _summonSpell2Cast :: Int,
    _superMonsterKilled :: Int,
    _team :: Int,
    _teamObjective :: Int,
    _timePlayed :: Int,
    _totalDamageDealt :: Int,
    _totalDamageDealtToChampions :: Int,
    _totalDamageTaken :: Int,
    _totalHeal :: Int,
    _totalPlayerScore :: Int,
    _totalScoreRank :: Int,
    _totalTimeCrowdControlDealt :: Int,
    _totalUnitsHealed :: Int,
    _tripleKills :: Int,
    _trueDamageDealtPlayer :: Int,
    _trueDamageDealtToChampions :: Int,
    _trueDamageTaken :: Int,
    _turretsKilled :: Int,
    _unrealKills :: Int,
    _victoryPointTotal :: Int,
    _visionWardsBought :: Int,
    _wardKilled :: Int,
    _wardPlaced :: Int,
    _win :: Bool
} deriving (Eq, Show)

makeLenses ''RawStatsDto

instance FromJSON RawStatsDto where
    parseJSON (Object v) = RawStatsDto <$>
        v .: "assists"<*>
        v .: "barracksKilled"<*>
        v .: "championsKilled"<*>
        v .: "combatPlayerScore"<*>
        v .: "consumablesPurchased"<*>
        v .: "damageDealtPlayer"<*>
        v .: "doubleKills"<*>
        v .: "firstBlood"<*>
        v .: "gold"<*>
        v .: "goldEarned"<*>
        v .: "goldSpent"<*>
        v .: "item0"<*>
        v .: "item1"<*>
        v .: "item2"<*>
        v .: "item3"<*>
        v .: "item4"<*>
        v .: "item5"<*>
        v .: "item6"<*>
        v .: "itemsPurchased"<*>
        v .: "killingSprees"<*>
        v .: "largestCriticalStrike"<*>
        v .: "largestKillingSpree"<*>
        v .: "largestMultiKill"<*>
        v .: "legendaryItemsCreated"<*>
        v .: "level"<*>
        v .: "magicDamageDealtPlayer"<*>
        v .: "magicDamageDealtToChampions"<*>
        v .: "magicDamageTaken"<*>
        v .: "minionsDenied"<*>
        v .: "minionsKilled"<*>
        v .: "neutralMinionsKilled"<*>
        v .: "neutralMinionsKilledEnemyJungle"<*>
        v .: "neutralMinionsKilledYourJungle"<*>
        v .: "nexusKilled"<*>
        v .: "nodeCapture"<*>
        v .: "nodeCaptureAssist"<*>
        v .: "nodeNeutralize"<*>
        v .: "nodeNeutralizeAssist"<*>
        v .: "numDeaths"<*>
        v .: "numItemsBought"<*>
        v .: "objectivePlayerScore"<*>
        v .: "pentaKills"<*>
        v .: "physicalDamageDealtPlayer"<*>
        v .: "physicalDamageDealtToChampions"<*>
        v .: "physicalDamageTaken"<*>
        v .: "quadraKills"<*>
        v .: "sightWardsBought"<*>
        v .: "spell1Cast"<*>
        v .: "spell2Cast"<*>
        v .: "spell3Cast"<*>
        v .: "spell4Cast"<*>
        v .: "summonSpell1Cast"<*>
        v .: "summonSpell2Cast"<*>
        v .: "superMonsterKilled"<*>
        v .: "team"<*>
        v .: "teamObjective"<*>
        v .: "timePlayed"<*>
        v .: "totalDamageDealt"<*>
        v .: "totalDamageDealtToChampions"<*>
        v .: "totalDamageTaken"<*>
        v .: "totalHeal"<*>
        v .: "totalPlayerScore"<*>
        v .: "totalScoreRank"<*>
        v .: "totalTimeCrowdControlDealt"<*>
        v .: "totalUnitsHealed"<*>
        v .: "tripleKills"<*>
        v .: "trueDamageDealtPlayer"<*>
        v .: "trueDamageDealtToChampions"<*>
        v .: "trueDamageTaken"<*>
        v .: "turretsKilled"<*>
        v .: "unrealKills"<*>
        v .: "victoryPointTotal"<*>
        v .: "visionWardsBought"<*>
        v .: "wardKilled"<*>
        v .: "wardPlaced"<*>
        v .: "win"
    parseJSON _ = mzero

data GameDto = GameDto {
    _championId :: Int,
    _createDate :: Int,
    _fellowPlayers :: [PlayerDto],
    _gameId :: Int,
    _gameMode :: String,
    _gameType :: String,
    _invalid :: Bool,
    _ipEarned :: Int,
    _gameLevel :: Int,
    _mapId :: Int,
    _spell1 :: Int,
    _spell2 :: Int,
    _stats :: RawStatsDto,
    _subType :: String,
    _teamId :: Int
} deriving (Eq, Show)

makeLenses ''GameDto

instance FromJSON GameDto where
    parseJSON (Object v) = GameDto <$>
        v .: "championId"<*>
        v .: "createDate"<*>
        v .: "fellowPlayers"<*>
        v .: "gameId"<*>
        v .: "gameMode"<*>
        v .: "gameType"<*>
        v .: "invalid"<*>
        v .: "ipEarned"<*>
        v .: "level"<*>
        v .: "mapId"<*>
        v .: "spell1"<*>
        v .: "spell2"<*>
        v .: "stats"<*>
        v .: "subType"<*>
        v .: "teamId"
    parseJSON _ = mzero

data RecentGamesDto = RecentGamesDto {
    _games :: [GameDto],
    _summonerId :: Int
}

makeLenses ''RecentGamesDto

instance FromJSON RecentGamesDto where
    parseJSON (Object v) = RecentGamesDto <$>
        v .: "games" <*>
        v .: "summonerId"
    parseJSON _ = mzero
