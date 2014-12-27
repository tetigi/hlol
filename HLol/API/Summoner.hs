module HLol.API.Summoner (
    getByNames
    ) where

import HLol.Data
import HLol.Network.Rest

import Data.Aeson
import Data.List (intercalate)
import qualified Data.Map as M

requestByNames :: [String] -> IO (M.Map String Summoner)
requestByNames names = do
    let url = "/v1.4/summoner/by-name/" ++ intercalate "," names
    resp <- sendAPIRequest url []
    case eitherDecode resp of
        Right summoners  -> return summoners
        Left e -> error $ "Error: Could not parse summoners from reponse " ++ show resp ++ ", got " ++ e

getByNames :: [String] -> IO [Summoner]
getByNames names = fmap M.elems $ requestByNames names
