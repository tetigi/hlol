module HLol.API.Summoner (
    getByNames
    ) where

import HLol.Data
import HLol.Network.Rest

import Data.Aeson
import Data.ByteString.Lazy.Char8 (pack)
import Data.List (intercalate)
import qualified Data.Map as M

requestByNames :: [String] -> IO (M.Map String Summoner)
requestByNames names = do
    let url = "/v1.4/summoner/by-name/" ++ intercalate "," names
    resp <- sendAPIRequest url []
    case decode $ pack resp of
        Just summoners  -> return summoners
        Nothing         -> error $ "Error: Could not parse summoners from reponse " ++ resp

getByNames :: [String] -> IO [Summoner]
getByNames names = fmap M.elems $ requestByNames names
