module HLol.API.Summoner (
    getByNames
    ) where

import HLol.Data.Summoner
import HLol.Network.Rest

import Data.List (intercalate)
import qualified Data.Map as M

getByNames :: [String] -> IO (Either LolError (M.Map String SummonerDto))
getByNames names = get $ "/v1.4/summoner/by-name/" ++ intercalate "," names
