{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HLol.Data.LolStatus where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson

data Shard = Shard {
    _shardHostname :: String,
    _shardLocales :: [String],
    _shardName :: String,
    _shardRegion_tag :: Maybe String,
    _shardSlug :: String
} deriving (Eq, Show)

makeLenses ''Shard

instance FromJSON Shard where
    parseJSON (Object v) = Shard <$>
        v .: "hostname"<*>
        v .: "locales"<*>
        v .: "name"<*>
        v .:? "region_tag"<*>
        v .: "slug"
    parseJSON _ = mzero

data Translation = Translation {
    _content :: String,
    _locale :: String,
    _updatedAt :: String
} deriving (Eq, Show)

makeLenses ''Translation

instance FromJSON Translation where
    parseJSON (Object v) = Translation <$>
        v .: "content"<*>
        v .: "locale"<*>
        v .: "updated_at"
    parseJSON _ = mzero

data Message = Message {
    _messageAuthor :: String,
    _messageContent :: String,
    _messageCreatedAt :: String,
    _messageId :: Int,
    _messageSeverity :: String,
    _messageTranslations :: [Translation],
    _messageUpdatedAt :: String
} deriving (Eq, Show)

makeLenses ''Message

instance FromJSON Message where
    parseJSON (Object v) = Message <$>
        v .: "author"<*>
        v .: "content"<*>
        v .: "created_at"<*>
        v .: "id"<*>
        v .: "severity"<*>
        v .: "translations"<*>
        v .: "updated_at"
    parseJSON _ = mzero

data Incident = Incident {
    _active :: Bool,
    _created_at :: String,
    _id :: Int,
    _updates :: [Message]
} deriving (Eq, Show)

makeLenses ''Incident

instance FromJSON Incident where
    parseJSON (Object v) = Incident <$>
        v .: "active"<*>
        v .: "created_at"<*>
        v .: "id"<*>
        v .: "updates"
    parseJSON _ = mzero

data Service = Service {
    _serviceIncidents :: [Incident],
    _serviceName :: String,
    _serviceSlug :: String,
    _serviceStatus :: String
} deriving (Eq, Show)

makeLenses ''Service

instance FromJSON Service where
    parseJSON (Object v) = Service <$>
        v .: "incidents"<*>
        v .: "name"<*>
        v .: "slug"<*>
        v .: "status"
    parseJSON _ = mzero

data ShardStatus = ShardStatus {
    _shardStatusHostname :: String,
    _shardStatusLocales :: [String],
    _shardStatusName :: String,
    _shardStatusRegion_tag :: String,
    _shardStatusServices :: [Service],
    _shardStatusSlug :: String
} deriving (Eq, Show)

makeLenses ''ShardStatus

instance FromJSON ShardStatus where
    parseJSON (Object v) = ShardStatus <$>
        v .: "hostname"<*>
        v .: "locales"<*>
        v .: "name"<*>
        v .: "region_tag"<*>
        v .: "services"<*>
        v .: "slug"
    parseJSON _ = mzero

