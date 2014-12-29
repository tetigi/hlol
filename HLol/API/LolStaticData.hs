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

import Data.Aeson
import Data.List (intercalate)
import qualified Data.Map as M

getChampions :: IO ChampionListDto
getChampions = get "/v1.2/champion"

getChampion :: IO Int -> ChampionDto
getChampion champId = get $ "/v1.2/champion/" ++ show champId

getItems :: IO ItemListDto
getItems = get "/v1.2/item"

getItem :: IO Int -> ItemDto
getItem itemId = get $ "/v1.2/item/" ++ show itemId

getLanguageData :: IO [String]
getLanguageData = get "/v1.2/languages"

getMasteries :: IO MasteryListDto
getMasteries = get "/v1.2/mastery"

getMastery :: IO Int -> MasteryDto
getMastery masteryId = get $ "/v1.2/mastery/" ++ show masteryId

getRealmData :: IO RealmDto
getRealmData = get "/v1.2/realm"

getRunes :: IO RuneListDto
getRunes = get "/v1.2/rune"

getRune :: IO Int -> RuneDto
getRune runeId = get $ "/v1.2/rune/" ++ show runeId

getSummonerSpells :: IO SummonerSpellListDto
getSummonerSpells = get "/v1.2/summoner-spell"

getSummonerSpell :: IO Int -> SummonerSpellDto
getSummonerSpell spellId = get $ "/v1.2/summoner-spell/" ++ show spellId

getVersionData :: IO [String]
getVersionData = get "/v1.2/versions"
