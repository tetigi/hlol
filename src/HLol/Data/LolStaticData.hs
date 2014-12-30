{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HLol.Data.LolStaticData where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson
import qualified Data.Map as M

import HLol.Data.Champion (ChampionDto)

data BlockItemDto = BlockItemDto {
    _count :: Int,
    _blockItemId :: Int
} deriving (Eq, Show)

makeLenses ''BlockItemDto

instance FromJSON BlockItemDto where
    parseJSON (Object v) = BlockItemDto <$>
        v .: "count"<*>
        v .: "id"
    parseJSON _ = mzero
data BlockDto = BlockDto {
    _items :: [BlockItemDto],
    _recMath :: Bool,
    _blockType :: String
} deriving (Eq, Show)

makeLenses ''BlockDto

instance FromJSON BlockDto where
    parseJSON (Object v) = BlockDto <$>
        v .: "items"<*>
        v .: "recMath"<*>
        v .: "type"
    parseJSON _ = mzero
data SpellVarsDto = SpellVarsDto {
    _coeff :: [Double],
    _dyn :: String,
    _spellKey :: String,
    _link :: String,
    _ranksWith :: String
} deriving (Eq, Show)

makeLenses ''SpellVarsDto

instance FromJSON SpellVarsDto where
    parseJSON (Object v) = SpellVarsDto <$>
        v .: "coeff"<*>
        v .: "dyn"<*>
        v .: "key"<*>
        v .: "link"<*>
        v .: "ranksWith"
    parseJSON _ = mzero

data LevelTipDto = LevelTipDto {
    _tipEffect :: [String],
    _tipLabel :: [String]
} deriving (Eq, Show)

makeLenses ''LevelTipDto

instance FromJSON LevelTipDto where
    parseJSON (Object v) = LevelTipDto <$>
        v .: "effect"<*>
        v .: "label"
    parseJSON _ = mzero
data StatsDto = StatsDto {
    _armor :: Double,
    _armorperlevel :: Double,
    _attackdamage :: Double,
    _attackdamageperlevel :: Double,
    _attackrange :: Double,
    _attackspeedoffset :: Double,
    _attackspeedperlevel :: Double,
    _crit :: Double,
    _critperlevel :: Double,
    _hp :: Double,
    _hpperlevel :: Double,
    _hpregen :: Double,
    _hpregenperlevel :: Double,
    _movespeed :: Double,
    _mp :: Double,
    _mpperlevel :: Double,
    _mpregen :: Double,
    _mpregenperlevel :: Double,
    _spellblock :: Double,
    _spellblockperlevel :: Double
} deriving (Eq, Show)

makeLenses ''StatsDto

instance FromJSON StatsDto where
    parseJSON (Object v) = StatsDto <$>
        v .: "armor"<*>
        v .: "armorperlevel"<*>
        v .: "attackdamage"<*>
        v .: "attackdamageperlevel"<*>
        v .: "attackrange"<*>
        v .: "attackspeedoffset"<*>
        v .: "attackspeedperlevel"<*>
        v .: "crit"<*>
        v .: "critperlevel"<*>
        v .: "hp"<*>
        v .: "hpperlevel"<*>
        v .: "hpregen"<*>
        v .: "hpregenperlevel"<*>
        v .: "movespeed"<*>
        v .: "mp"<*>
        v .: "mpperlevel"<*>
        v .: "mpregen"<*>
        v .: "mpregenperlevel"<*>
        v .: "spellblock"<*>
        v .: "spellblockperlevel"
    parseJSON _ = mzero
data SkinDto = SkinDto {
    _id :: Int,
    _skinName :: String,
    _num :: Int
} deriving (Eq, Show)

makeLenses ''SkinDto

instance FromJSON SkinDto where
    parseJSON (Object v) = SkinDto <$>
        v .: "id"<*>
        v .: "name"<*>
        v .: "num"
    parseJSON _ = mzero
data RecommendedDto = RecommendedDto {
    _blocks :: [BlockDto],
    _champion :: String,
    _map :: String,
    _mode :: String,
    _priority :: Bool,
    _title :: String,
    _type :: String
} deriving (Eq, Show)

makeLenses ''RecommendedDto

instance FromJSON RecommendedDto where
    parseJSON (Object v) = RecommendedDto <$>
        v .: "blocks"<*>
        v .: "champion"<*>
        v .: "map"<*>
        v .: "mode"<*>
        v .: "priority"<*>
        v .: "title"<*>
        v .: "type"
    parseJSON _ = mzero

data InfoDto = InfoDto {
    _attack :: Int,
    _defense :: Int,
    _difficulty :: Int,
    _magic :: Int
} deriving (Eq, Show)

makeLenses ''InfoDto

instance FromJSON InfoDto where
    parseJSON (Object v) = InfoDto <$>
        v .: "attack"<*>
        v .: "defense"<*>
        v .: "difficulty"<*>
        v .: "magic"
    parseJSON _ = mzero

data ImageDto = ImageDto {
    _full :: String,
    _group :: String,
    _h :: Int,
    _sprite :: String,
    _w :: Int,
    _x :: Int,
    _y :: Int
} deriving (Eq, Show)

makeLenses ''ImageDto

instance FromJSON ImageDto where
    parseJSON (Object v) = ImageDto <$>
        v .: "full"<*>
        v .: "group"<*>
        v .: "h"<*>
        v .: "sprite"<*>
        v .: "w"<*>
        v .: "x"<*>
        v .: "y"
    parseJSON _ = mzero

data PassiveDto = PassiveDto {
    _passiveDescription :: String,
    _passiveImage :: ImageDto,
    _passiveName :: String,
    _passiveSanitizedDescription :: String
} deriving (Eq, Show)

makeLenses ''PassiveDto

instance FromJSON PassiveDto where
    parseJSON (Object v) = PassiveDto <$>
        v .: "description"<*>
        v .: "image"<*>
        v .: "name"<*>
        v .: "sanitizedDescription"
    parseJSON _ = mzero

data ChampionSpellDto = ChampionSpellDto {
    _altimages :: [ImageDto],
    _cooldown :: [Double],
    _cooldownBurn :: String,
    _cost :: [Int],
    _costBurn :: String,
    _costType :: String,
    _description :: String,
    _effect :: [[Double]],
    _effectBurn :: [String],
    _image :: ImageDto,
    _key :: String,
    _leveltip :: LevelTipDto,
    _maxrank :: Int,
    _name :: String,
    _range :: Either [Int] String,
    _rangeBurn :: String,
    _resource :: String,
    _sanitizedDescription :: String,
    _sanitizedTooltip :: String,
    _tooltip :: String,
    _vars :: [SpellVarsDto]
} deriving (Eq, Show)

makeLenses ''ChampionSpellDto

instance FromJSON ChampionSpellDto where
    parseJSON (Object v) = ChampionSpellDto <$>
        v .: "altimages"<*>
        v .: "cooldown"<*>
        v .: "cooldownBurn"<*>
        v .: "cost"<*>
        v .: "costBurn"<*>
        v .: "costType"<*>
        v .: "description"<*>
        v .: "effect"<*>
        v .: "effectBurn"<*>
        v .: "image"<*>
        v .: "key"<*>
        v .: "leveltip"<*>
        v .: "maxrank"<*>
        v .: "name"<*>
        v .: "range"<*>
        v .: "rangeBurn"<*>
        v .: "resource"<*>
        v .: "sanitizedDescription"<*>
        v .: "sanitizedTooltip"<*>
        v .: "tooltip"<*>
        v .: "vars"
    parseJSON _ = mzero
data ChampionListDto = ChampionListDto {
    _data :: M.Map String ChampionDto,
    _format :: String,
    _keys :: M.Map String String,
    _listType :: String,
    _version :: String
} deriving (Eq, Show)

makeLenses ''ChampionListDto

instance FromJSON ChampionListDto where
    parseJSON (Object v) = ChampionListDto <$>
        v .: "data"<*>
        v .: "format"<*>
        v .: "keys"<*>
        v .: "type"<*>
        v .: "version"
    parseJSON _ = mzero

data MetaDataDto = MetaDataDto {
    _isRune :: Bool,
    _tier :: String,
    _metaDataType :: String
} deriving (Eq, Show)

makeLenses ''MetaDataDto

instance FromJSON MetaDataDto where
    parseJSON (Object v) = MetaDataDto <$>
        v .: "isRune"<*>
        v .: "tier"<*>
        v .: "type"
    parseJSON _ = mzero

data GoldDto = GoldDto {
    _base :: Int,
    _purchasable :: Bool,
    _sell :: Int,
    _total :: Int
} deriving (Eq, Show)

makeLenses ''GoldDto

instance FromJSON GoldDto where
    parseJSON (Object v) = GoldDto <$>
        v .: "base"<*>
        v .: "purchasable"<*>
        v .: "sell"<*>
        v .: "total"
    parseJSON _ = mzero
data BasicDataStatsDto = BasicDataStatsDto {
    _FlatArmorMod :: Double,
    _FlatAttackSpeedMod :: Double,
    _FlatBlockMod :: Double,
    _FlatCritChanceMod :: Double,
    _FlatCritDamageMod :: Double,
    _FlatEXPBonus :: Double,
    _FlatEnergyPoolMod :: Double,
    _FlatEnergyRegenMod :: Double,
    _FlatHPPoolMod :: Double,
    _FlatHPRegenMod :: Double,
    _FlatMPPoolMod :: Double,
    _FlatMPRegenMod :: Double,
    _FlatMagicDamageMod :: Double,
    _FlatMovementSpeedMod :: Double,
    _FlatPhysicalDamageMod :: Double,
    _FlatSpellBlockMod :: Double,
    _PercentArmorMod :: Double,
    _PercentAttackSpeedMod :: Double,
    _PercentBlockMod :: Double,
    _PercentCritChanceMod :: Double,
    _PercentCritDamageMod :: Double,
    _PercentDodgeMod :: Double,
    _PercentEXPBonus :: Double,
    _PercentHPPoolMod :: Double,
    _PercentHPRegenMod :: Double,
    _PercentLifeStealMod :: Double,
    _PercentMPPoolMod :: Double,
    _PercentMPRegenMod :: Double,
    _PercentMagicDamageMod :: Double,
    _PercentMovementSpeedMod :: Double,
    _PercentPhysicalDamageMod :: Double,
    _PercentSpellBlockMod :: Double,
    _PercentSpellVampMod :: Double,
    _rFlatArmorModPerLevel :: Double,
    _rFlatArmorPenetrationMod :: Double,
    _rFlatArmorPenetrationModPerLevel :: Double,
    _rFlatCritChanceModPerLevel :: Double,
    _rFlatCritDamageModPerLevel :: Double,
    _rFlatDodgeMod :: Double,
    _rFlatDodgeModPerLevel :: Double,
    _rFlatEnergyModPerLevel :: Double,
    _rFlatEnergyRegenModPerLevel :: Double,
    _rFlatGoldPer10Mod :: Double,
    _rFlatHPModPerLevel :: Double,
    _rFlatHPRegenModPerLevel :: Double,
    _rFlatMPModPerLevel :: Double,
    _rFlatMPRegenModPerLevel :: Double,
    _rFlatMagicDamageModPerLevel :: Double,
    _rFlatMagicPenetrationMod :: Double,
    _rFlatMagicPenetrationModPerLevel :: Double,
    _rFlatMovementSpeedModPerLevel :: Double,
    _rFlatPhysicalDamageModPerLevel :: Double,
    _rFlatSpellBlockModPerLevel :: Double,
    _rFlatTimeDeadMod :: Double,
    _rFlatTimeDeadModPerLevel :: Double,
    _rPercentArmorPenetrationMod :: Double,
    _rPercentArmorPenetrationModPerLevel :: Double,
    _rPercentAttackSpeedModPerLevel :: Double,
    _rPercentCooldownMod :: Double,
    _rPercentCooldownModPerLevel :: Double,
    _rPercentMagicPenetrationMod :: Double,
    _rPercentMagicPenetrationModPerLevel :: Double,
    _rPercentMovementSpeedModPerLevel :: Double,
    _rPercentTimeDeadMod :: Double,
    _rPercentTimeDeadModPerLevel :: Double
} deriving (Eq, Show)

makeLenses ''BasicDataStatsDto

instance FromJSON BasicDataStatsDto where
    parseJSON (Object v) = BasicDataStatsDto <$>
        v .: "FlatArmorMod"<*>
        v .: "FlatAttackSpeedMod"<*>
        v .: "FlatBlockMod"<*>
        v .: "FlatCritChanceMod"<*>
        v .: "FlatCritDamageMod"<*>
        v .: "FlatEXPBonus"<*>
        v .: "FlatEnergyPoolMod"<*>
        v .: "FlatEnergyRegenMod"<*>
        v .: "FlatHPPoolMod"<*>
        v .: "FlatHPRegenMod"<*>
        v .: "FlatMPPoolMod"<*>
        v .: "FlatMPRegenMod"<*>
        v .: "FlatMagicDamageMod"<*>
        v .: "FlatMovementSpeedMod"<*>
        v .: "FlatPhysicalDamageMod"<*>
        v .: "FlatSpellBlockMod"<*>
        v .: "PercentArmorMod"<*>
        v .: "PercentAttackSpeedMod"<*>
        v .: "PercentBlockMod"<*>
        v .: "PercentCritChanceMod"<*>
        v .: "PercentCritDamageMod"<*>
        v .: "PercentDodgeMod"<*>
        v .: "PercentEXPBonus"<*>
        v .: "PercentHPPoolMod"<*>
        v .: "PercentHPRegenMod"<*>
        v .: "PercentLifeStealMod"<*>
        v .: "PercentMPPoolMod"<*>
        v .: "PercentMPRegenMod"<*>
        v .: "PercentMagicDamageMod"<*>
        v .: "PercentMovementSpeedMod"<*>
        v .: "PercentPhysicalDamageMod"<*>
        v .: "PercentSpellBlockMod"<*>
        v .: "PercentSpellVampMod"<*>
        v .: "rFlatArmorModPerLevel"<*>
        v .: "rFlatArmorPenetrationMod"<*>
        v .: "rFlatArmorPenetrationModPerLevel"<*>
        v .: "rFlatCritChanceModPerLevel"<*>
        v .: "rFlatCritDamageModPerLevel"<*>
        v .: "rFlatDodgeMod"<*>
        v .: "rFlatDodgeModPerLevel"<*>
        v .: "rFlatEnergyModPerLevel"<*>
        v .: "rFlatEnergyRegenModPerLevel"<*>
        v .: "rFlatGoldPer10Mod"<*>
        v .: "rFlatHPModPerLevel"<*>
        v .: "rFlatHPRegenModPerLevel"<*>
        v .: "rFlatMPModPerLevel"<*>
        v .: "rFlatMPRegenModPerLevel"<*>
        v .: "rFlatMagicDamageModPerLevel"<*>
        v .: "rFlatMagicPenetrationMod"<*>
        v .: "rFlatMagicPenetrationModPerLevel"<*>
        v .: "rFlatMovementSpeedModPerLevel"<*>
        v .: "rFlatPhysicalDamageModPerLevel"<*>
        v .: "rFlatSpellBlockModPerLevel"<*>
        v .: "rFlatTimeDeadMod"<*>
        v .: "rFlatTimeDeadModPerLevel"<*>
        v .: "rPercentArmorPenetrationMod"<*>
        v .: "rPercentArmorPenetrationModPerLevel"<*>
        v .: "rPercentAttackSpeedModPerLevel"<*>
        v .: "rPercentCooldownMod"<*>
        v .: "rPercentCooldownModPerLevel"<*>
        v .: "rPercentMagicPenetrationMod"<*>
        v .: "rPercentMagicPenetrationModPerLevel"<*>
        v .: "rPercentMovementSpeedModPerLevel"<*>
        v .: "rPercentTimeDeadMod"<*>
        v .: "rPercentTimeDeadModPerLevel"
    parseJSON _ = mzero
data ItemTreeDto = ItemTreeDto {
    _header :: String,
    _tags :: [String]
} deriving (Eq, Show)

makeLenses ''ItemTreeDto

instance FromJSON ItemTreeDto where
    parseJSON (Object v) = ItemTreeDto <$>
        v .: "header"<*>
        v .: "tags"
    parseJSON _ = mzero
data ItemDto = ItemDto {
    _colloq :: String,
    _consumeOnFull :: Bool,
    _consumed :: Bool,
    _depth :: Int,
    _itemDescription :: String,
    _itemEffect :: M.Map String String,
    _from :: [String],
    _gold :: GoldDto,
    _itemGroup :: String,
    _hideFromAll :: Bool,
    _itemId :: Int,
    _itemImage :: ImageDto,
    _inStore :: Bool,
    _into :: [String],
    _maps :: M.Map String Bool,
    _itemName :: String,
    _plaintext :: String,
    _requiredChampion :: String,
    _rune :: MetaDataDto,
    _itemSanitizedDescription :: String,
    _specialRecipe :: Int,
    _stacks :: Int,
    _stats :: BasicDataStatsDto,
    _itemTags :: [String]
} deriving (Eq, Show)

makeLenses ''ItemDto

instance FromJSON ItemDto where
    parseJSON (Object v) = ItemDto <$>
        v .: "colloq"<*>
        v .: "consumeOnFull"<*>
        v .: "consumed"<*>
        v .: "depth"<*>
        v .: "description"<*>
        v .: "effect"<*>
        v .: "from"<*>
        v .: "gold"<*>
        v .: "group"<*>
        v .: "hideFromAll"<*>
        v .: "id"<*>
        v .: "image"<*>
        v .: "inStore"<*>
        v .: "into"<*>
        v .: "maps"<*>
        v .: "name"<*>
        v .: "plaintext"<*>
        v .: "requiredChampion"<*>
        v .: "rune"<*>
        v .: "sanitizedDescription"<*>
        v .: "specialRecipe"<*>
        v .: "stacks"<*>
        v .: "stats"<*>
        v .: "tags"
    parseJSON _ = mzero

data GroupDto = GroupDto {
    _MaxGroupOwnable :: String,
    _groupKey :: String
} deriving (Eq, Show)

makeLenses ''GroupDto

instance FromJSON GroupDto where
    parseJSON (Object v) = GroupDto <$>
        v .: "MaxGroupOwnable"<*>
        v .: "key"
    parseJSON _ = mzero

data BasicDataDto = BasicDataDto {
    _bdataColloq :: String,
    _bdataConsumeOnFull :: Bool,
    _bdataConsumed :: Bool,
    _bdataDepth :: Int,
    _bdataDescription :: String,
    _bdataFrom :: [String],
    _bdataGold :: GoldDto,
    _bdataGroup :: String,
    _bdataHideFromAll :: Bool,
    _bdataId :: Int,
    _bdataImage :: ImageDto,
    _bdataInStore :: Bool,
    _bdataInto :: [String],
    _bdataMaps :: M.Map String Bool,
    _bdataName :: String,
    _bataPlaintext :: String,
    _bdataRequiredChampion :: String,
    _bdataRune :: MetaDataDto,
    _bdataSanitizedDescription :: String,
    _bdataSpecialRecipe :: Int,
    _bdataStacks :: Int,
    _bdataStats :: BasicDataStatsDto,
    _bdataTags :: [String]
} deriving (Eq, Show)

makeLenses ''BasicDataDto

instance FromJSON BasicDataDto where
    parseJSON (Object v) = BasicDataDto <$>
        v .: "colloq"<*>
        v .: "consumeOnFull"<*>
        v .: "consumed"<*>
        v .: "depth"<*>
        v .: "description"<*>
        v .: "from"<*>
        v .: "gold"<*>
        v .: "group"<*>
        v .: "hideFromAll"<*>
        v .: "id"<*>
        v .: "image"<*>
        v .: "inStore"<*>
        v .: "into"<*>
        v .: "maps"<*>
        v .: "name"<*>
        v .: "plaintext"<*>
        v .: "requiredChampion"<*>
        v .: "rune"<*>
        v .: "sanitizedDescription"<*>
        v .: "specialRecipe"<*>
        v .: "stacks"<*>
        v .: "stats"<*>
        v .: "tags"
    parseJSON _ = mzero
data ItemListDto = ItemListDto {
    _basic :: BasicDataDto,
    _itemListData :: M.Map String ItemDto,
    _groups :: [GroupDto],
    _tree :: [ItemTreeDto],
    _itemListType :: String,
    _itemListVersion :: String
} deriving (Eq, Show)

makeLenses ''ItemListDto

instance FromJSON ItemListDto where
    parseJSON (Object v) = ItemListDto <$>
        v .: "basic"<*>
        v .: "data"<*>
        v .: "groups"<*>
        v .: "tree"<*>
        v .: "type"<*>
        v .: "version"
    parseJSON _ = mzero

data MasteryTreeItemDto = MasteryTreeItemDto {
    _masteryTreeItemId :: Int,
    _masteryTreeItemPrereq :: String
} deriving (Eq, Show)

makeLenses ''MasteryTreeItemDto

instance FromJSON MasteryTreeItemDto where
    parseJSON (Object v) = MasteryTreeItemDto <$>
        v .: "masteryId"<*>
        v .: "prereq"
    parseJSON _ = mzero
data MasteryTreeListDto = MasteryTreeListDto {
    _masteryTreeItems :: [MasteryTreeItemDto]
} deriving (Eq, Show)

makeLenses ''MasteryTreeListDto

data MasteryTreeDto = MasteryTreeDto {
    _masteryTreeDefense :: [MasteryTreeListDto],
    _masteryTreeOffense :: [MasteryTreeListDto],
    _masteryTreeUtility :: [MasteryTreeListDto]
} deriving (Eq, Show)

makeLenses ''MasteryTreeDto

instance FromJSON MasteryTreeDto where
    parseJSON (Object v) = MasteryTreeDto <$>
        v .: "Defense"<*>
        v .: "Offense"<*>
        v .: "Utility"
    parseJSON _ = mzero

instance FromJSON MasteryTreeListDto where
    parseJSON (Object v) = MasteryTreeListDto <$>
        v .: "masteryTreeItems"
    parseJSON _ = mzero

data MasteryDto = MasteryDto {
    _masteryId :: Int,
    _masteryRank :: Int
} deriving (Eq, Show)

makeLenses ''MasteryDto

instance FromJSON MasteryDto where
    parseJSON (Object v) = MasteryDto <$>
        v .: "id"<*>
        v .: "rank"
    parseJSON _ = mzero

data MasteryListDto = MasteryListDto {
    _masteryListData :: M.Map String MasteryDto,
    _masteryListTree :: MasteryTreeDto,
    _masteryListType :: String,
    _masteryListVersion :: String
} deriving (Eq, Show)

makeLenses ''MasteryListDto

instance FromJSON MasteryListDto where
    parseJSON (Object v) = MasteryListDto <$>
        v .: "data"<*>
        v .: "tree"<*>
        v .: "type"<*>
        v .: "version"
    parseJSON _ = mzero

data RealmDto = RealmDto {
    _cdn :: String,
    _css :: String,
    _dd :: String,
    _l :: String,
    _lg :: String,
    _n :: M.Map String String,
    _profileiconmax :: Int,
    _store :: String,
    _v :: String
} deriving (Eq, Show)

makeLenses ''RealmDto

instance FromJSON RealmDto where
    parseJSON (Object o) = RealmDto <$>
        o .: "cdn"<*>
        o .: "css"<*>
        o .: "dd"<*>
        o .: "l"<*>
        o .: "lg"<*>
        o .: "n"<*>
        o .: "profileiconmax"<*>
        o .: "store"<*>
        o .: "v"
    parseJSON _ = mzero

data RuneDto = RuneDto {
    _runeColloq :: String,
    _runeConsumeOnFull :: Bool,
    _runeConsumed :: Bool,
    _runeDepth :: Int,
    _runeDescription :: String,
    _runeFrom :: [String],
    _runeGold :: GoldDto,
    _runeGroup :: String,
    _runeHideFromAll :: Bool,
    _runeId :: Int,
    _runeImage :: ImageDto,
    _runeInStore :: Bool,
    _runeInto :: [String],
    _runeMaps :: M.Map String Bool,
    _runeName :: String,
    _runePlaintext :: String,
    _runeRequiredChampion :: String,
    _runeRune :: MetaDataDto,
    _runeSanitizedDescription :: String,
    _runeSpecialRecipe :: Int,
    _runeStacks :: Int,
    _runeStats :: BasicDataStatsDto,
    _runeTags :: [String]
} deriving (Eq, Show)

makeLenses ''RuneDto

instance FromJSON RuneDto where
    parseJSON (Object o) = RuneDto <$>
        o .: "colloq"<*>
        o .: "consumeOnFull"<*>
        o .: "consumed"<*>
        o .: "depth"<*>
        o .: "description"<*>
        o .: "from"<*>
        o .: "gold"<*>
        o .: "group"<*>
        o .: "hideFromAll"<*>
        o .: "id"<*>
        o .: "image"<*>
        o .: "inStore"<*>
        o .: "into"<*>
        o .: "maps"<*>
        o .: "name"<*>
        o .: "plaintext"<*>
        o .: "requiredChampion"<*>
        o .: "rune"<*>
        o .: "sanitizedDescription"<*>
        o .: "specialRecipe"<*>
        o .: "stacks"<*>
        o .: "stats"<*>
        o .: "tags"
    parseJSON _ = mzero

data RuneListDto = RuneListDto {
    _runeListBasic :: BasicDataDto,
    _runeListData :: M.Map String RuneDto,
    _runeListType :: String,
    _runeListVersion :: String
} deriving (Eq, Show)

makeLenses ''RuneListDto

instance FromJSON RuneListDto where
    parseJSON (Object o) = RuneListDto <$>
        o .: "basic"<*>
        o .: "data"<*>
        o .: "type"<*>
        o .: "version"
    parseJSON _ = mzero

data SummonerSpellDto = SummonerSpellDto {
    _summonerSpellCooldown :: [Double],
    _summonerSpellCooldownBurn :: String,
    _summonerSpellCost :: [Int],
    _summonerSpellCostBurn :: String,
    _summonerSpellCostType :: String,
    _summonerSpellDescription :: String,
    _summonerSpellEffect :: [[Double]],
    _summonerSpellEffectBurn :: [String],
    _summonerSpellId :: Int,
    _summonerSpellImage :: ImageDto,
    _summonerSpellKey :: String,
    _summonerSpellLeveltip :: LevelTipDto,
    _summonerSpellMaxrank :: Int,
    _summonerSpellModes :: [String],
    _summonerSpellName :: String,
    _summonerSpellRange :: Either [Int] String,
    _summonerSpellRangeBurn :: String,
    _summonerSpellResource :: String,
    _summonerSpellSanitizedDescription :: String,
    _summonerSpellSanitizedTooltip :: String,
    _summonerSpellSummonerLevel :: Int,
    _summonerSpellTooltip :: String,
    _summonerSpellVars :: [SpellVarsDto]
} deriving (Eq, Show)

makeLenses ''SummonerSpellDto

instance FromJSON SummonerSpellDto where
    parseJSON (Object o) = SummonerSpellDto <$>
        o .: "cooldown"<*>
        o .: "cooldownBurn"<*>
        o .: "cost"<*>
        o .: "costBurn"<*>
        o .: "costType"<*>
        o .: "description"<*>
        o .: "effect"<*>
        o .: "effectBurn"<*>
        o .: "id"<*>
        o .: "image"<*>
        o .: "key"<*>
        o .: "leveltip"<*>
        o .: "maxrank"<*>
        o .: "modes"<*>
        o .: "name"<*>
        o .: "range"<*>
        o .: "rangeBurn"<*>
        o .: "resource"<*>
        o .: "sanitizedDescription"<*>
        o .: "sanitizedTooltip"<*>
        o .: "summonerLevel"<*>
        o .: "tooltip"<*>
        o .: "vars"
    parseJSON _ = mzero

data SummonerSpellListDto = SummonerSpellListDto {
    _summonerSpellListData :: M.Map String SummonerSpellDto,
    _summonerSpellListType :: String,
    _summonerSpellListVersion :: String
} deriving (Eq, Show)

makeLenses ''SummonerSpellListDto

instance FromJSON SummonerSpellListDto where
    parseJSON (Object o) = SummonerSpellListDto <$>
        o .: "data"<*>
        o .: "type"<*>
        o .: "version"
    parseJSON _ = mzero
