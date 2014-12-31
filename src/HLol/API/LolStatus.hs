module HLol.API.LolStatus (
    getShards,
    getShardByRegion
    ) where

import HLol.Data.LolStatus (Shard, ShardStatus)
import HLol.Network.Rest
import HLol.Utils

import Data.Aeson

getShards :: IO (Either LolError [Shard])
getShards =
    fmap (mapR (getRight . eitherDecode)) $ sendAPIRequest_ "http://status.leagueoflegends.com/shards"

getShardByRegion :: Region -> IO (Either LolError ShardStatus)
getShardByRegion region =
    fmap (mapR (getRight . eitherDecode)) $ sendAPIRequest_ $ "http://status.leagueoflegends.com/shards/" ++ show region
