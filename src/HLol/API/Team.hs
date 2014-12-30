module HLol.API.Team (
    getTeamsBySummonerIds,
    getTeamsByTeamIds
    ) where

import HLol.Data.Team (TeamDto)
import HLol.Network.Rest

import Data.List (intercalate)
import qualified Data.Map as M

getTeamsBySummonerIds :: [Int] -> IO (Either LolError (M.Map String [TeamDto]))
getTeamsBySummonerIds summonerIds =
    get $ "/v2.4/team/by-summoner/" ++ (intercalate "," $ map show summonerIds)

getTeamsByTeamIds :: [Int] -> IO (Either LolError (M.Map String [TeamDto]))
getTeamsByTeamIds teamIds =
    get $ "/v2.4/team/" ++ (intercalate "," $ map show teamIds)
