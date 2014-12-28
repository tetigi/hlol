{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HLol.Data.Summoner where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson

data SummonerDto = SummonerDto {
    _id :: Int,
    _name :: String,
    _profileIconId :: Int,
    _revisionDate :: Int,
    _summonerLevel :: Int
} deriving (Eq, Show)

makeLenses ''SummonerDto

instance FromJSON SummonerDto where
    parseJSON (Object v) = SummonerDto <$>
        v .: "id"<*>
        v .: "name"<*>
        v .: "profileIconId"<*>
        v .: "revisionDate"<*>
        v .: "summonerLevel"
    parseJSON _ = mzero
