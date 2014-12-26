{-# LANGUAGE OverloadedStrings #-}

module HLol.Data where

import Control.Monad
import Control.Applicative
import Data.Aeson

data Summoner = Summoner {
    summonerName :: String,
    summonerId :: Int,
    profileIconId :: Int,
    revisionDate :: Int,
    summonerLevel :: Int
} deriving (Eq, Show)

instance FromJSON Summoner where
    parseJSON (Object v) =  Summoner <$>
                            v .: "name" <*>
                            v .: "id" <*>
                            v .: "profileIconId" <*>
                            v .: "revisionDate" <*>
                            v .: "summonerLevel"
    parseJSON _          = mzero
