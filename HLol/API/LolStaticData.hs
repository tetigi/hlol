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

getChampions :: IO ChampionListDto
getChampions = get "/v1.2/champion"

getChampion :: Int -> IO ChampionDto
getChampion champId = get $ "/v1.2/champion/" ++ show champId

getItems :: IO ItemListDto
getItems = get "/v1.2/item"

getItem :: Int -> IO ItemDto
getItem item = get $ "/v1.2/item/" ++ show item

getLanguageData :: IO [String]
getLanguageData = get "/v1.2/languages"

getMasteries :: IO MasteryListDto
getMasteries = get "/v1.2/mastery"

getMastery :: Int -> IO MasteryDto
getMastery master = get $ "/v1.2/mastery/" ++ show master

getRealmData :: IO RealmDto
getRealmData = get "/v1.2/realm"

getRunes :: IO RuneListDto
getRunes = get "/v1.2/rune"

getRune :: Int -> IO RuneDto
getRune runeID = get $ "/v1.2/rune/" ++ show runeID

getSummonerSpells :: IO SummonerSpellListDto
getSummonerSpells = get "/v1.2/summoner-spell"

getSummonerSpell :: Int -> IO SummonerSpellDto
getSummonerSpell spellId = get $ "/v1.2/summoner-spell/" ++ show spellId

getVersionData :: IO [String]
getVersionData = get "/v1.2/versions"
