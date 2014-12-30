module HLol.API.MatchHistory (
    getMatchHistory
    ) where

import HLol.Data.MatchHistory (PlayerHistory)
import HLol.Network.Rest
import HLol.Utils

import Data.Aeson
import qualified Data.Map as M

getMatchHistory :: Int -> Int -> IO (Either LolError PlayerHistory)
getMatchHistory matchId limit = do
    let url = "/v2.2/matchhistory/" ++ show matchId
    resp <- sendAPIRequest url [("endIndex", show limit)]
    return $ mapR (getRight . eitherDecode) resp
