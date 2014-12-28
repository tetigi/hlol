{-# LANGUAGE OverloadedStrings #-}
module HLol.API.Match (
    getMatch
    ) where

import HLol.Data.Match (MatchDetail(..))
import HLol.Network.Rest

import Data.Aeson

requestMatch :: Int -> IO MatchDetail
requestMatch matchId = do
    let url = "/v2.2/match/" ++ show matchId
    resp <- sendAPIRequest url []
    case eitherDecode resp of
        Right r -> return r
        Left e  -> error e

getMatch :: Int -> IO MatchDetail
getMatch = requestMatch
