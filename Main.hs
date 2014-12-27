module Main where

import HLol.API.MatchHistory
import HLol.API.Match
import HLol.API.Summoner
import HLol.Data
import HLol.Logging.Logger

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad
import qualified Data.Set as S

tag :: String
tag = "Main"

main :: IO ()
main = do
    putStrLn "Hello, world!"
    [tetigi] <- getByNames ["tetigi"]
    withLogging $ gatherNames S.empty [tetigi^.summonerId]

gatherNames :: S.Set Int -> [Int] -> IO ()
gatherNames _ [] = error "No seed names provided!"
gatherNames seen (n:ns) = do
    matches <- getMatchHistory n 5
    matchDetails <- mapM (getMatch . (^.matchId)) matches
    let players = concat $ map (map (^.player) . (^.participantIdentities)) matchDetails
    (seen', names') <- logNames (seen, ns) players
    threadDelay 20000000
    gatherNames seen' names'

logNames :: (S.Set Int, [Int]) -> [Player] -> IO (S.Set Int, [Int])
logNames = foldM add
    where
        add (ss, ns) p
            | (p^.playerId) `S.notMember` ss = do
                lolInfo tag $ (p^.playerName) ++ "," ++ show (p^.playerId)
                return (S.insert (p^.playerId) ss, (p^.playerId):ns)
            | otherwise = return (ss, ns)
