module HLol.API.League (
    getLeagues,
    getLeagueEntries,
    getLeaguesByTeam,
    getLeagueEntriesByTeam,
    getChallengerLeagues
    ) where

import HLol.Data.League (LeagueDto)
import HLol.Network.Rest

import Data.List (intercalate)
import qualified Data.Map as M

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

getChallengerLeagues :: IO (Either LolError LeagueDto)
getChallengerLeagues = get "/v2.5/league/challenger"
