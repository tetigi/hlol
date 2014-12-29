module Main where

import HLol.API.MatchHistory
import HLol.API.Match
import HLol.API.Summoner
import HLol.API.Champion
import HLol.API.Game
import HLol.API.League
import HLol.API.LolStaticData
import HLol.API.LolStatus
import HLol.API.Stats
import HLol.API.Team

{-
import qualified HLol.Data.Match as Match
import qualified HLol.Data.Summoner as Summoner
import qualified HLol.Data.MatchHistory as MatchHistory
import qualified HLol.Data.Champion as Champion
import qualified HLol.Data.Game as Game

import HLol.Logging.Logger

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad
import qualified Data.Set as S

tag :: String
tag = "Main"
-}

main :: IO ()
main = do
    putStrLn "Hello, world!"

{-
    [tetigi] <- getByNames ["tetigi"]
    withLogging $ gatherNames S.empty [tetigi^.Summoner.id]

gatherNames :: S.Set Int -> [Int] -> IO ()
gatherNames _ [] = error "No seed names provided!"
gatherNames seen (n:ns) = do
    matches <- getMatchHistory n 5
    matchDetails <- mapM (getMatch . (^.MatchHistory.matchId)) matches
    let players = concat $ map (map (^.Match.player) . (^.Match.participantIdentities)) matchDetails
    (seen', names') <- logNames (seen, ns) players
    threadDelay 20000000
    gatherNames seen' names'

logNames :: (S.Set Int, [Int]) -> [Match.Player] -> IO (S.Set Int, [Int])
logNames = foldM add
    where
        add (ss, ns) p
            | (p^.Match.summonerId) `S.notMember` ss = do
                lolInfo tag $ (p^.Match.summonerName) ++ "," ++ show (p^.Match.summonerId)
                return (S.insert (p^.Match.summonerId) ss, (p^.Match.summonerId):ns)
            | otherwise = return (ss, ns)
-}
