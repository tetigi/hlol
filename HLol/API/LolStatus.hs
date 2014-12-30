module HLol.API.LolStatus (
    getShards,
    getShardByRegion
    ) where

import HLol.Data.LolStatus (Shard, ShardStatus)
import HLol.Network.Rest
import HLol.Utils

import Data.Aeson

import Network.Curl ( curlGetString_ )
import Network.Curl.Code ( CurlCode( CurlOK ) )

getShards :: IO (Either LolError [Shard])
getShards =
    fmap (mapR (getRight . eitherDecode)) $ sendAPIRequest' "http://status.leagueoflegends.com/shards"

getShardByRegion :: Region -> IO (Either LolError ShardStatus)
getShardByRegion region =
    fmap (mapR (getRight . eitherDecode)) $ sendAPIRequest' $ "http://status.leagueoflegends.com/shards/" ++ show region
