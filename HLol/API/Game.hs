module HLol.API.Game (
    getGames
    ) where

import HLol.Data.Game (RecentGamesDto)
import HLol.Network.Rest

import Data.Aeson

getGames :: Int -> IO RecentGamesDto
getGames summonerId = get $ "/v1.3/game/by-summoner/" ++ show summonerId ++ "/recent"
