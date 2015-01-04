module HLol.API.LolStaticData (
    getChampions,
    getChampion,
    getItems,
    getItem,
    getLanguageData,
    getMasteries,
    getMastery,
    getRealmData,
    getRunes,
    getRune,
    getSummonerSpells,
    getSummonerSpell,
    getVersionData
    ) where

import HLol.Data.LolStaticData
import HLol.Network.Rest
import HLol.Utils

import Control.Monad (join)
import Data.Aeson

static_base :: Region -> String
static_base r = "https://global.api.pvp.net/api/lol/static-data/" ++ show r

getStatic :: (FromJSON a) => String -> IO (Either LolError a)
getStatic url = do
    resp <- sendAPIRequest' static_base url []
    return $ join $ mapR (liftError . eitherDecode) resp

getStaticWithOpts :: (FromJSON a) => String -> [(String, String)] -> IO (Either LolError a)
getStaticWithOpts url opts = do
    resp <- sendAPIRequest' static_base url opts
    return $ join $ mapR (liftError . eitherDecode) resp

getChampions :: IO (Either LolError ChampionListDto)
getChampions = getStaticWithOpts "/v1.2/champion" [("champData", "all")]

getChampion :: Int -> IO (Either LolError ChampionDto)
getChampion champId = getStaticWithOpts ("/v1.2/champion/" ++ show champId) [("champData", "all")]

getItems :: IO (Either LolError ItemListDto)
getItems = getStaticWithOpts "/v1.2/item" [("itemListData", "all")]

getItem :: Int -> IO (Either LolError ItemDto)
getItem item = getStaticWithOpts ("/v1.2/item/" ++ show item) [("itemData", "all")]

getLanguageData :: IO (Either LolError [String])
getLanguageData = getStatic "/v1.2/languages"

getMasteries :: IO (Either LolError MasteryListDto)
getMasteries = getStaticWithOpts "/v1.2/mastery" [("masteryListData", "all")]

getMastery :: Int -> IO (Either LolError MasteryDto)
getMastery master = getStaticWithOpts ("/v1.2/mastery/" ++ show master) [("masteryData", "all")]

getRealmData :: IO (Either LolError RealmDto)
getRealmData = getStatic "/v1.2/realm"

getRunes :: IO (Either LolError RuneListDto)
getRunes = getStaticWithOpts "/v1.2/rune" [("runeListData", "all")]

getRune :: Int -> IO (Either LolError RuneDto)
getRune runeID = getStaticWithOpts ("/v1.2/rune/" ++ show runeID) [("runeData", "all")]

getSummonerSpells :: IO (Either LolError SummonerSpellListDto)
getSummonerSpells = getStaticWithOpts "/v1.2/summoner-spell" [("spellData", "all")]

getSummonerSpell :: Int -> IO (Either LolError SummonerSpellDto)
getSummonerSpell spellId = getStaticWithOpts ("/v1.2/summoner-spell/" ++ show spellId) [("spellData", "all")]

getVersionData :: IO (Either LolError [String])
getVersionData = getStatic "/v1.2/versions"
