{-# LANGUAGE OverloadedStrings #-}
module HLol.API.Match (
    getMatch
    ) where

import HLol.Data.Match (MatchDetail(..))
import HLol.Network.Rest

getMatch :: Int -> IO (Either LolError MatchDetail)
getMatch matchId = getWithOpts ("/v2.2/match/" ++ show matchId) [("includeTimeline", "true")]
