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

import Control.Monad
import Control.Applicative
import Data.Either

import qualified Test.HUnit as HU
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid

assertRight :: Either a b -> HU.Assertion
assertRight e = unless (isRight e) (HU.assertFailure "Expected Right but got Left")

testC = buildTest $ fmap (testCase "championGet" . assertRight) $ Champion.getChampions False

-- champion-v1.2
-- game-v1.3
-- league-v2.5
-- lol-static-data-v1.2
-- lol-status-v1.0
-- match-v2.2
-- matchhistory-v2.2
-- stats-v1.3
-- summoner-v1.4
-- team-v2.4

ioTests = testGroup "IO Tests"
    [
       testC
    ]

main :: IO ()
main = do
    defaultMainWithOpts [ioTests] mempty
