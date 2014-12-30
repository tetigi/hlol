{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HLol.Data.Stats where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson

data AggregatedStatsDto = AggregatedStatsDto {
    _averageAssists :: Int,
    _averageChampionsKilled :: Int,
    _averageCombatPlayerScore :: Int,
    _averageNodeCapture :: Int,
    _averageNodeCaptureAssist :: Int,
    _averageNodeNeutralize :: Int,
    _averageNodeNeutralizeAssist :: Int,
    _averageNumDeaths :: Int,
    _averageObjectivePlayerScore :: Int,
    _averageTeamObjective :: Int,
    _averageTotalPlayerScore :: Int,
    _botGamesPlayed :: Int,
    _killingSpree :: Int,
    _maxAssists :: Int,
    _maxChampionsKilled :: Int,
    _maxCombatPlayerScore :: Int,
    _maxLargestCriticalStrike :: Int,
    _maxLargestKillingSpree :: Int,
    _maxNodeCapture :: Int,
    _maxNodeCaptureAssist :: Int,
    _maxNodeNeutralize :: Int,
    _maxNodeNeutralizeAssist :: Int,
    _maxNumDeaths :: Int,
    _maxObjectivePlayerScore :: Int,
    _maxTeamObjective :: Int,
    _maxTimePlayed :: Int,
    _maxTimeSpentLiving :: Int,
    _maxTotalPlayerScore :: Int,
    _mostChampionKillsPerSession :: Int,
    _mostSpellsCast :: Int,
    _normalGamesPlayed :: Int,
    _rankedPremadeGamesPlayed :: Int,
    _rankedSoloGamesPlayed :: Int,
    _totalAssists :: Int,
    _totalChampionKills :: Int,
    _totalDamageDealt :: Int,
    _totalDamageTaken :: Int,
    _totalDeathsPerSession :: Int,
    _totalDoubleKills :: Int,
    _totalFirstBlood :: Int,
    _totalGoldEarned :: Int,
    _totalHeal :: Int,
    _totalMagicDamageDealt :: Int,
    _totalMinionKills :: Int,
    _totalNeutralMinionsKilled :: Int,
    _totalNodeCapture :: Int,
    _totalNodeNeutralize :: Int,
    _totalPentaKills :: Int,
    _totalPhysicalDamageDealt :: Int,
    _totalQuadraKills :: Int,
    _totalSessionsLost :: Int,
    _totalSessionsPlayed :: Int,
    _totalSessionsWon :: Int,
    _totalTripleKills :: Int,
    _totalTurretsKilled :: Int,
    _totalUnrealKills :: Int
} deriving (Eq, Show)

makeLenses ''AggregatedStatsDto

instance FromJSON AggregatedStatsDto where
    parseJSON (Object v) = AggregatedStatsDto <$>
        v .: "averageAssists"<*>
        v .: "averageChampionsKilled"<*>
        v .: "averageCombatPlayerScore"<*>
        v .: "averageNodeCapture"<*>
        v .: "averageNodeCaptureAssist"<*>
        v .: "averageNodeNeutralize"<*>
        v .: "averageNodeNeutralizeAssist"<*>
        v .: "averageNumDeaths"<*>
        v .: "averageObjectivePlayerScore"<*>
        v .: "averageTeamObjective"<*>
        v .: "averageTotalPlayerScore"<*>
        v .: "botGamesPlayed"<*>
        v .: "killingSpree"<*>
        v .: "maxAssists"<*>
        v .: "maxChampionsKilled"<*>
        v .: "maxCombatPlayerScore"<*>
        v .: "maxLargestCriticalStrike"<*>
        v .: "maxLargestKillingSpree"<*>
        v .: "maxNodeCapture"<*>
        v .: "maxNodeCaptureAssist"<*>
        v .: "maxNodeNeutralize"<*>
        v .: "maxNodeNeutralizeAssist"<*>
        v .: "maxNumDeaths"<*>
        v .: "maxObjectivePlayerScore"<*>
        v .: "maxTeamObjective"<*>
        v .: "maxTimePlayed"<*>
        v .: "maxTimeSpentLiving"<*>
        v .: "maxTotalPlayerScore"<*>
        v .: "mostChampionKillsPerSession"<*>
        v .: "mostSpellsCast"<*>
        v .: "normalGamesPlayed"<*>
        v .: "rankedPremadeGamesPlayed"<*>
        v .: "rankedSoloGamesPlayed"<*>
        v .: "totalAssists"<*>
        v .: "totalChampionKills"<*>
        v .: "totalDamageDealt"<*>
        v .: "totalDamageTaken"<*>
        v .: "totalDeathsPerSession"<*>
        v .: "totalDoubleKills"<*>
        v .: "totalFirstBlood"<*>
        v .: "totalGoldEarned"<*>
        v .: "totalHeal"<*>
        v .: "totalMagicDamageDealt"<*>
        v .: "totalMinionKills"<*>
        v .: "totalNeutralMinionsKilled"<*>
        v .: "totalNodeCapture"<*>
        v .: "totalNodeNeutralize"<*>
        v .: "totalPentaKills"<*>
        v .: "totalPhysicalDamageDealt"<*>
        v .: "totalQuadraKills"<*>
        v .: "totalSessionsLost"<*>
        v .: "totalSessionsPlayed"<*>
        v .: "totalSessionsWon"<*>
        v .: "totalTripleKills"<*>
        v .: "totalTurretsKilled"<*>
        v .: "totalUnrealKills"
    parseJSON _ = mzero
data ChampionStatsDto = ChampionStatsDto {
    _id :: Int,
    _stats :: AggregatedStatsDto
} deriving (Eq, Show)

makeLenses ''ChampionStatsDto

instance FromJSON ChampionStatsDto where
    parseJSON (Object v) = ChampionStatsDto <$>
        v .: "id"<*>
        v .: "stats"
    parseJSON _ = mzero
data RankedStatsDto = RankedStatsDto {
    _rankedChampions :: [ChampionStatsDto],
    _rankedModifyDate :: Int,
    _rankedSummonerId :: Int
} deriving (Eq, Show)

makeLenses ''RankedStatsDto

instance FromJSON RankedStatsDto where
    parseJSON (Object v) = RankedStatsDto <$>
        v .: "champions"<*>
        v .: "modifyDate"<*>
        v .: "summonerId"
    parseJSON _ = mzero

data PlayerStatsSummaryDto = PlayerStatsSummaryDto {
    _aggregatedStats :: AggregatedStatsDto,
    _losses :: Int,
    _modifyDate :: Int,
    _playerStatSummaryType :: String,
    _wins :: Int
} deriving (Eq, Show)

makeLenses ''PlayerStatsSummaryDto

instance FromJSON PlayerStatsSummaryDto where
    parseJSON (Object v) = PlayerStatsSummaryDto <$>
        v .: "aggregatedStats"<*>
        v .: "losses"<*>
        v .: "modifyDate"<*>
        v .: "playerStatSummaryType"<*>
        v .: "wins"
    parseJSON _ = mzero

data PlayerStatsSummaryListDto = PlayerStatsSummaryListDto {
    _playerStatSummaries :: [PlayerStatsSummaryDto],
    _summonerId :: Int
} deriving (Eq, Show)

makeLenses ''PlayerStatsSummaryListDto

instance FromJSON PlayerStatsSummaryListDto where
    parseJSON (Object v) = PlayerStatsSummaryListDto <$>
        v .: "playerStatSummaries"<*>
        v .: "summonerId"
    parseJSON _ = mzero
