module HLol.API.LolStatus (
    getShards,
    getShardByRegion
    ) where

import HLol.Data.LolStatus (Shard, ShardStatus)
import HLol.Network.Rest
import HLol.Utils

import Control.Monad
import Data.Aeson

getShards :: IO (Either LolError [Shard])
getShards =
    fmap (join . mapR (liftError. eitherDecode)) $ sendAPIRequest_ "http://status.leagueoflegends.com/shards"

getShardByRegion :: Region -> IO (Either LolError ShardStatus)
getShardByRegion region =
    fmap (join . mapR (liftError . eitherDecode)) $ sendAPIRequest_ $ "http://status.leagueoflegends.com/shards/" ++ show region
