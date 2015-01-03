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
import HLol.Data.Champion
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
getChampion champId = getStatic $ "/v1.2/champion/" ++ show champId

getItems :: IO (Either LolError ItemListDto)
getItems = getStaticWithOpts "/v1.2/item" [("itemListData", "all")]

getItem :: Int -> IO (Either LolError ItemDto)
getItem item = getStatic $ "/v1.2/item/" ++ show item

getLanguageData :: IO (Either LolError [String])
getLanguageData = getStatic "/v1.2/languages"

getMasteries :: IO (Either LolError MasteryListDto)
getMasteries = getStatic "/v1.2/mastery"

getMastery :: Int -> IO (Either LolError MasteryDto)
getMastery master = getStatic $ "/v1.2/mastery/" ++ show master

getRealmData :: IO (Either LolError RealmDto)
getRealmData = getStatic "/v1.2/realm"

getRunes :: IO (Either LolError RuneListDto)
getRunes = getStatic "/v1.2/rune"

getRune :: Int -> IO (Either LolError RuneDto)
getRune runeID = getStatic $ "/v1.2/rune/" ++ show runeID

getSummonerSpells :: IO (Either LolError SummonerSpellListDto)
getSummonerSpells = getStatic "/v1.2/summoner-spell"

getSummonerSpell :: Int -> IO (Either LolError SummonerSpellDto)
getSummonerSpell spellId = getStatic $ "/v1.2/summoner-spell/" ++ show spellId

getVersionData :: IO (Either LolError [String])
getVersionData = getStatic "/v1.2/versions"
