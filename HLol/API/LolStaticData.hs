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

getChampions :: IO (Either LolError ChampionListDto)
getChampions = get "/v1.2/champion"

getChampion :: Int -> IO (Either LolError ChampionDto)
getChampion champId = get $ "/v1.2/champion/" ++ show champId

getItems :: IO (Either LolError ItemListDto)
getItems = get "/v1.2/item"

getItem :: Int -> IO (Either LolError ItemDto)
getItem item = get $ "/v1.2/item/" ++ show item

getLanguageData :: IO (Either LolError [String])
getLanguageData = get "/v1.2/languages"

getMasteries :: IO (Either LolError MasteryListDto)
getMasteries = get "/v1.2/mastery"

getMastery :: Int -> IO (Either LolError MasteryDto)
getMastery master = get $ "/v1.2/mastery/" ++ show master

getRealmData :: IO (Either LolError RealmDto)
getRealmData = get "/v1.2/realm"

getRunes :: IO (Either LolError RuneListDto)
getRunes = get "/v1.2/rune"

getRune :: Int -> IO (Either LolError RuneDto)
getRune runeID = get $ "/v1.2/rune/" ++ show runeID

getSummonerSpells :: IO (Either LolError SummonerSpellListDto)
getSummonerSpells = get "/v1.2/summoner-spell"

getSummonerSpell :: Int -> IO (Either LolError SummonerSpellDto)
getSummonerSpell spellId = get $ "/v1.2/summoner-spell/" ++ show spellId

getVersionData :: IO (Either LolError [String])
getVersionData = get "/v1.2/versions"
