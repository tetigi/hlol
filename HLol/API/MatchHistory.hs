module HLol.API.MatchHistory (
    getMatchHistory
    ) where

import HLol.Data.MatchHistory (MatchSummary)
import HLol.Network.Rest

import Data.Aeson
import qualified Data.Map as M

requestMatchHistory :: Int -> Int -> IO (M.Map String [MatchSummary])
requestMatchHistory matchId limit = do
    let url = "/v2.2/matchhistory/" ++ show matchId
    resp <- sendAPIRequest url [("endIndex", show limit)]
    case eitherDecode resp of
        Right matches -> return matches
        Left e -> error $ "Error: Could not parse match history from reponse " ++ show resp ++ ", got " ++ e

getMatchHistory :: Int -> Int -> IO [MatchSummary]
getMatchHistory matchId = fmap (M.! "matches") . requestMatchHistory matchId
