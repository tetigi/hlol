module HLol.API.Stats (
    getRankedStats,
    getSummaries
    ) where

import HLol.Data.Stats (RankedStatsDto, PlayerStatsSummaryListDto)
import HLol.Network.Rest

import Data.Aeson

getRankedStats :: Int -> IO (Either LolError RankedStatsDto)
getRankedStats summonerId = get $ "/v1.3/stats/by-summoner/" ++ show summonerId ++ "/ranked"

getSummaries :: Int -> IO (Either LolError PlayerStatsSummaryListDto)
getSummaries summonerId = get $ "/v1.3/stats/by-summoner/" ++ show summonerId ++ "/summary"
