module HLol.API.Champion (
    getChampions
    ) where

import HLol.Data.Champion (ChampionDto)
import HLol.Network.Rest
import HLol.Utils

import Data.Aeson
import qualified Data.Map as M

requestChampions :: Bool -> IO (Either LolError (M.Map String ChampionDto))
requestChampions freeToPlay = do
    let url = "/v1.2/champion"
    resp <- sendAPIRequest url [("freeToPlay", show freeToPlay)]
    return $ mapR (getRight . eitherDecode) resp

getChampions :: Bool -> IO (Either LolError ChampionDto)
getChampions = fmap (mapR (M.! "champions")) . requestChampions
