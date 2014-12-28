{-# LANGUAGE OverloadedStrings #-}
module HLol.API.Champion (
    getChampions
    ) where

import HLol.Data.Champion (ChampionDto)
import HLol.Network.Rest

import Data.Aeson
import qualified Data.Map as M

requestMatch :: Bool -> IO [ChampionDto]
requestMatch freeToPlay = do
    let url = "/v1.2/champion"
    resp <- sendAPIRequest url [("freeToPlay", show freeToPlay)]
    case eitherDecode resp of
        Right r -> return r
        Left e  -> error e

getMatch :: Int -> IO ChampionDto
getMatch = fmap (M.! "champions") . requestMatch
