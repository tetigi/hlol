module HLol.API.League (
    getLeagues,
    getLeagueEntries,
    getLeaguesByTeam,
    getLeagueEntriesByTeam,
    getChallengerLeagues,
    LeagueType(..)
    ) where

import HLol.Data.League (LeagueDto)
import HLol.Network.Rest
import HLol.Utils

import Data.Aeson
import Data.List (intercalate)
import qualified Data.Map as M

data LeagueType = RankedSolo5x5 | RankedTeam3x3 | RankedTeam5x5 deriving Eq

instance (Show LeagueType) where
    show RankedSolo5x5 = "RANKED_SOLO_5x5"
    show RankedTeam3x3 = "RANKED_TEAM_3x3"
    show RankedTeam5x5 = "RANKED_TEAM_5x5"

getLeagues :: [Int] -> IO (Either LolError (M.Map String [LeagueDto]))
getLeagues summonerIds =
    get $ "/v2.5/league/by-summoner/" ++ (intercalate "," $ map show summonerIds)

getLeagueEntries :: [Int] -> IO (Either LolError (M.Map String [LeagueDto]))
getLeagueEntries summonerIds =
    get $ "/v2.5/league/by-summoner/" ++ (intercalate "," $ map show summonerIds) ++ "/entry"

getLeaguesByTeam :: [Int] -> IO (Either LolError (M.Map String [LeagueDto]))
getLeaguesByTeam teamIds =
    get $ "/v2.5/league/by-team/" ++ (intercalate "," $ map show teamIds)

getLeagueEntriesByTeam :: [Int] -> IO (Either LolError (M.Map String [LeagueDto]))
getLeagueEntriesByTeam teamIds =
    get $ "/v2.5/league/by-team/" ++ (intercalate "," $ map show teamIds) ++ "/entry"

getChallengerLeagues :: LeagueType -> IO (Either LolError LeagueDto)
getChallengerLeagues league = do
    resp <- sendAPIRequest "/v2.5/league/challenger" [("type", show league)]
    return $ mapR (getRight . eitherDecode) resp
