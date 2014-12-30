{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HLol.Data.Champion where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson

data ChampionDto = ChampionDto {
    _active :: Bool,
    _botEnabled :: Bool,
    _botMmEnabled :: Bool,
    _freeToPlay :: Bool,
    _championId :: Int,
    _rankedPlayEnabled :: Bool
} deriving (Eq, Show)

makeLenses ''ChampionDto

instance FromJSON ChampionDto where
    parseJSON (Object v) = ChampionDto <$>
                            v .: "active" <*>
                            v .: "botEnabled" <*>
                            v .: "botMmEnabled" <*>
                            v .: "freeToPlay" <*>
                            v .: "id" <*>
                            v .: "rankedPlayEnabled"
    parseJSON _          = mzero
