module HLol.API.LolStatus (
    getShards,
    getShardByRegion
    ) where

import HLol.Data.LolStatus (Shard, ShardStatus)
import HLol.Network.Rest (Region)

import Data.Aeson

import Network.Curl ( curlGetString_ )
import Network.Curl.Code ( CurlCode( CurlOK ) )

getShards :: IO [Shard]
getShards = do
    (code, output) <- curlGetString_ "http://status.leagueoflegends.com/shards" []
    case code of
        CurlOK  ->  case eitherDecode output of
                        Right r -> return r
                        Left e -> error e
        _       -> error $ "Error: Could not get services. Got response: " ++ show code ++ "\n" ++ show output

getShardByRegion :: Region -> IO ShardStatus
getShardByRegion region = do
    (code, output) <- curlGetString_ ("http://status.leagueoflegends.com/shards/" ++ show region) []
    case code of
        CurlOK  ->  case eitherDecode output of
                        Right r -> return r
                        Left e -> error e
        _       -> error $ "Error: Could not get services. Got response: " ++ show code ++ "\n" ++ show output
