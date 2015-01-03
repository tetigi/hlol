module Main where

import qualified HLol.API.MatchHistory as MatchHistory
import qualified HLol.API.Match as Match
import qualified HLol.API.Summoner as Summoner
import qualified HLol.API.Champion as Champion
import qualified HLol.API.Game as Game
import qualified HLol.API.League as League
import qualified HLol.API.LolStaticData as LolStaticData
import qualified HLol.API.LolStatus as LolStatus
import qualified HLol.API.Stats as Stats
import qualified HLol.API.Team as Team

import qualified HLol.Data.Summoner as Summ

import HLol.Network.Rest (Region(..))

import HLol.Logging.Logger
import HLol.Utils

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Either

import qualified Test.HUnit as HU
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import qualified Data.Map as M

assertRight :: Either a b -> HU.Assertion
assertRight e = unless (isRight e) (HU.assertFailure "Expected Right but got Left")

-- setup
tetigi :: Int
tetigi = 32569275

-- champion-v1.2 ------------------------
testChampionsGet = buildTest $ fmap (testCase "championsGet" . assertRight) $ Champion.getChampions False

-- game-v1.3 ----------------------------
testGameGet =
    buildTest $ fmap (testCase "gameGet" . assertRight) $ Game.getGames tetigi

-- league-v2.5 --------------------------

testLeagueId :: Int
testLeagueId = 19751358

testLeagueGet =
    buildTest $ fmap (testCase "leagueGet" . assertRight) $ League.getLeagues [testLeagueId]
testLeagueEntriesGet =
    buildTest $ fmap (testCase "leagueEntryGet" . assertRight) $ League.getLeagueEntries [testLeagueId]
{- TODO Get a team id
testLeaguesByTeamGet =
    buildTest $ fmap (testCase "leagueEntryGet" . assertRight) $ League.getLeagueEntries =<< sequence [tetigi]
testLeagueEntriesByTeamGet =
    buildTest $ fmap (testCase "leagueEntryGet" . assertRight) $ League.getLeagueEntries =<< sequence [tetigi]
-}
testChallengerGet = buildTest $ fmap (testCase "leagueChallengerGet" . assertRight) $ League.getChallengerLeagues League.RankedSolo5x5

-- lol-static-data-v1.2 ----------------
testChamp :: Int
testChamp = 266

testItem :: Int
testItem = undefined

testMastery :: Int
testMastery = undefined

testRune :: Int
testRune = undefined

testSummonerSpell :: Int
testSummonerSpell = undefined

testChampionGet = buildTest $ fmap (testCase "championGet" . assertRight) $ LolStaticData.getChampion testChamp
testItemsGet = buildTest $ fmap (testCase "itemsGet" . assertRight) $ LolStaticData.getItems
testItemGet = buildTest $ fmap (testCase "itemGet" . assertRight) $ LolStaticData.getItem testItem
testLanguageGet = buildTest $ fmap (testCase "languageGet" . assertRight) $ LolStaticData.getLanguageData
testMasteriesGet = buildTest $ fmap (testCase "masteriesGet" . assertRight) $ LolStaticData.getMasteries
testMasteryGet = buildTest $ fmap (testCase "masteryGet" . assertRight) $ LolStaticData.getMastery testMastery
testRealmDataGet = buildTest $ fmap (testCase "realmDataGet" . assertRight) $ LolStaticData.getRealmData
testRunesGet = buildTest $ fmap (testCase "runesGet" . assertRight) $ LolStaticData.getRunes
testRuneGet = buildTest $ fmap (testCase "runeGet" . assertRight) $ LolStaticData.getRune testRune
testSummonerSpellsGet =
    buildTest $ fmap (testCase "summonerSpellsGet" . assertRight) $ LolStaticData.getSummonerSpells
testSummonerSpellGet =
    buildTest $ fmap (testCase "summonerSpellGet" . assertRight) $ LolStaticData.getSummonerSpell testSummonerSpell
testVersionDataGet =
    buildTest $ fmap (testCase "versionDataGet" . assertRight) $ LolStaticData.getVersionData

-- lol-status-v1.0 ---------------------
testRegion :: Region
testRegion = EUW

testShardsGet = buildTest $ fmap (testCase "shardsGet" . assertRight) $ LolStatus.getShards
testShardByRegionGet = buildTest $ fmap (testCase "shardByRegionGet" . assertRight) $ LolStatus.getShardByRegion testRegion

-- match-v2.2 --------------------------
testMatch :: Int
testMatch = 1873964841

testMatchGet = buildTest $ fmap (testCase "matchGet" . assertRight) $ Match.getMatch testMatch

-- matchhistory-v2.2 -------------------
testMatchHistoryGet =
    buildTest $ fmap (testCase "matchHistoryGet" . assertRight) $ MatchHistory.getMatchHistory tetigi 10

-- stats-v1.3 --------------------------

testRankedStatsGet =
    buildTest $ fmap (testCase "rankedStatsGet" . assertRight) $ Stats.getRankedStats tetigi
testSummariesGet =
    buildTest $ fmap (testCase "summariesGet" . assertRight) $ Stats.getSummaries tetigi

-- summoner-v1.4 -----------------------

testByNameGet = buildTest $ fmap (testCase "byNameGet" . assertRight) $ Summoner.getByNames ["tetigi"]

-- team-v2.4 ---------------------------
testTeam :: String
testTeam = "TEAM-c27bcec0-8fe6-11e2-aa48-782bcb4ce61a"

testTeamBySummonerIdGet =
    buildTest $ fmap (testCase "teamBySummGet" . assertRight) $ Team.getTeamsBySummonerIds [tetigi]
testTeamByTeamIdGet =
    buildTest $ fmap (testCase "teamByTeamGet" . assertRight) $ Team.getTeamsByTeamIds [testTeam]

-- Test Aggregation ------------------------------------

championTests = testGroup "champion"
    [
        testChampionsGet
    ]
gameTests = testGroup "game"
    [
       testGameGet
    ]
leagueTests = testGroup "league"
    [
        testLeagueGet,
        testLeagueEntriesGet,
        testChallengerGet
    ]
lolStaticDataTests = testGroup "lol-static-data"
    [
        testChampionGet,
        testItemsGet,
        testItemGet,
        testMasteriesGet,
        testMasteryGet,
        testRunesGet,
        testRuneGet,
        testSummonerSpellsGet,
        testSummonerSpellGet,
        testRealmDataGet,
        testLanguageGet,
        testVersionDataGet
    ]
lolStatusTests = testGroup "lol-status"
    [
        testShardsGet,
        testShardByRegionGet
    ]
matchTests = testGroup "match"
    [
        testMatchGet
    ]
matchHistoryTests = testGroup "match-history"
    [
        testMatchHistoryGet
    ]
statsTests = testGroup "stats"
    [
        testRankedStatsGet,
        testSummariesGet
    ]
summonerTests = testGroup "summoner"
    [
        testByNameGet
    ]
teamTests = testGroup "team"
    [
        testTeamBySummonerIdGet,
        testTeamByTeamIdGet
    ]

allTests = [
    championTests,
    gameTests,
    leagueTests,
    --lolStaticDataTests,
    lolStatusTests,
    matchTests,
    matchHistoryTests,
    --statsTests,
    summonerTests,
    teamTests
    ]

main :: IO ()
main = do
    withLogging $ defaultMainWithOpts allTests mempty
