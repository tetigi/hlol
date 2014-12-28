{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module HLol.Data.Match where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Aeson

import HLol.Data.Champion (ChampionDto)

data BlockItemDto = BlockItemDto {
    _count :: Int,
    _id :: Int
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
    _type :: String
} deriving (Eq, Show)

makeLenses ''BlockDto

instance FromJSON BlockDto where
    parseJSON (Object v) = BlockDto <$>
        v .: "items"<*>
        v .: "recMath"<*>
        v .: "type"
    parseJSON _ = mzero
data SpellVarsDto = SpellVarsDto {
    _coeff :: [double],
    _dyn :: String,
    _key :: String,
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
    _effect :: [String],
    _label :: [String]
} deriving (Eq, Show)

makeLenses ''LevelTipDto

instance FromJSON LevelTipDto where
    parseJSON (Object v) = LevelTipDto <$>
        v .: "effect"<*>
        v .: "label"
    parseJSON _ = mzero
data StatsDto = StatsDto {
    _armor :: double,
    _armorperlevel :: double,
    _attackdamage :: double,
    _attackdamageperlevel :: double,
    _attackrange :: double,
    _attackspeedoffset :: double,
    _attackspeedperlevel :: double,
    _crit :: double,
    _critperlevel :: double,
    _hp :: double,
    _hpperlevel :: double,
    _hpregen :: double,
    _hpregenperlevel :: double,
    _movespeed :: double,
    _mp :: double,
    _mpperlevel :: double,
    _mpregen :: double,
    _mpregenperlevel :: double,
    _spellblock :: double,
    _spellblockperlevel :: double
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
    _name :: String,
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
data PassiveDto = PassiveDto {
    _description :: String,
    _image :: ImageDto,
    _name :: String,
    _sanitizedDescription :: String
} deriving (Eq, Show)

makeLenses ''PassiveDto

instance FromJSON PassiveDto where
    parseJSON (Object v) = PassiveDto <$>
        v .: "description"<*>
        v .: "image"<*>
        v .: "name"<*>
        v .: "sanitizedDescription"
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
data ChampionSpellDto = ChampionSpellDto {
    _altimages :: [ImageDto],
    _cooldown :: [double],
    _cooldownBurn :: String,
    _cost :: [Int],
    _costBurn :: String,
    _costType :: String,
    _description :: String,
    _effect :: [object],
    _effectBurn :: [String],
    _image :: ImageDto,
    _key :: String,
    _leveltip :: LevelTipDto,
    _maxrank :: Int,
    _name :: String,
    _range :: object,
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
    _data :: Map[string,,
    _format :: String,
    _keys :: Map[string,,
    _type :: String,
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
    _type :: String
} deriving (Eq, Show)

makeLenses ''MetaDataDto

instance FromJSON MetaDataDto where
    parseJSON (Object v) = MetaDataDto <$>
        v .: "isRune"<*>
        v .: "tier"<*>
        v .: "type"
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
    _FlatArmorMod :: double,
    _FlatAttackSpeedMod :: double,
    _FlatBlockMod :: double,
    _FlatCritChanceMod :: double,
    _FlatCritDamageMod :: double,
    _FlatEXPBonus :: double,
    _FlatEnergyPoolMod :: double,
    _FlatEnergyRegenMod :: double,
    _FlatHPPoolMod :: double,
    _FlatHPRegenMod :: double,
    _FlatMPPoolMod :: double,
    _FlatMPRegenMod :: double,
    _FlatMagicDamageMod :: double,
    _FlatMovementSpeedMod :: double,
    _FlatPhysicalDamageMod :: double,
    _FlatSpellBlockMod :: double,
    _PercentArmorMod :: double,
    _PercentAttackSpeedMod :: double,
    _PercentBlockMod :: double,
    _PercentCritChanceMod :: double,
    _PercentCritDamageMod :: double,
    _PercentDodgeMod :: double,
    _PercentEXPBonus :: double,
    _PercentHPPoolMod :: double,
    _PercentHPRegenMod :: double,
    _PercentLifeStealMod :: double,
    _PercentMPPoolMod :: double,
    _PercentMPRegenMod :: double,
    _PercentMagicDamageMod :: double,
    _PercentMovementSpeedMod :: double,
    _PercentPhysicalDamageMod :: double,
    _PercentSpellBlockMod :: double,
    _PercentSpellVampMod :: double,
    _rFlatArmorModPerLevel :: double,
    _rFlatArmorPenetrationMod :: double,
    _rFlatArmorPenetrationModPerLevel :: double,
    _rFlatCritChanceModPerLevel :: double,
    _rFlatCritDamageModPerLevel :: double,
    _rFlatDodgeMod :: double,
    _rFlatDodgeModPerLevel :: double,
    _rFlatEnergyModPerLevel :: double,
    _rFlatEnergyRegenModPerLevel :: double,
    _rFlatGoldPer10Mod :: double,
    _rFlatHPModPerLevel :: double,
    _rFlatHPRegenModPerLevel :: double,
    _rFlatMPModPerLevel :: double,
    _rFlatMPRegenModPerLevel :: double,
    _rFlatMagicDamageModPerLevel :: double,
    _rFlatMagicPenetrationMod :: double,
    _rFlatMagicPenetrationModPerLevel :: double,
    _rFlatMovementSpeedModPerLevel :: double,
    _rFlatPhysicalDamageModPerLevel :: double,
    _rFlatSpellBlockModPerLevel :: double,
    _rFlatTimeDeadMod :: double,
    _rFlatTimeDeadModPerLevel :: double,
    _rPercentArmorPenetrationMod :: double,
    _rPercentArmorPenetrationModPerLevel :: double,
    _rPercentAttackSpeedModPerLevel :: double,
    _rPercentCooldownMod :: double,
    _rPercentCooldownModPerLevel :: double,
    _rPercentMagicPenetrationMod :: double,
    _rPercentMagicPenetrationModPerLevel :: double,
    _rPercentMovementSpeedModPerLevel :: double,
    _rPercentTimeDeadMod :: double,
    _rPercentTimeDeadModPerLevel :: double
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
    _description :: String,
    _effect :: Map[string,,
    _from :: [String],
    _gold :: GoldDto,
    _group :: String,
    _hideFromAll :: Bool,
    _id :: Int,
    _image :: ImageDto,
    _inStore :: Bool,
    _into :: [String],
    _maps :: Map[string,,
    _name :: String,
    _plaintext :: String,
    _requiredChampion :: String,
    _rune :: MetaDataDto,
    _sanitizedDescription :: String,
    _specialRecipe :: Int,
    _stacks :: Int,
    _stats :: BasicDataStatsDto,
    _tags :: [String]
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
    _key :: String
} deriving (Eq, Show)

makeLenses ''GroupDto

instance FromJSON GroupDto where
    parseJSON (Object v) = GroupDto <$>
        v .: "MaxGroupOwnable"<*>
        v .: "key"
    parseJSON _ = mzero
data BasicDataDto = BasicDataDto {
    _colloq :: String,
    _consumeOnFull :: Bool,
    _consumed :: Bool,
    _depth :: Int,
    _description :: String,
    _from :: [String],
    _gold :: GoldDto,
    _group :: String,
    _hideFromAll :: Bool,
    _id :: Int,
    _image :: ImageDto,
    _inStore :: Bool,
    _into :: [String],
    _maps :: Map[string,,
    _name :: String,
    _plaintext :: String,
    _requiredChampion :: String,
    _rune :: MetaDataDto,
    _sanitizedDescription :: String,
    _specialRecipe :: Int,
    _stacks :: Int,
    _stats :: BasicDataStatsDto,
    _tags :: [String]
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
    _data :: Map[string,,
    _groups :: [GroupDto],
    _tree :: [ItemTreeDto],
    _type :: String,
    _version :: String
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
    _masteryId :: Int,
    _prereq :: String
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

instance FromJSON MasteryTreeListDto where
    parseJSON (Object v) = MasteryTreeListDto <$>
        v .: "masteryTreeItems"
    parseJSON _ = mzero
data MasteryListDto = MasteryListDto {
    _data :: Map[string,,
    _tree :: MasteryTreeDto,
    _type :: String,
    _version :: String
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
    _n :: Map[string,,
    _profileiconmax :: Int,
    _store :: String,
    _v :: String
} deriving (Eq, Show)

makeLenses ''RealmDto

instance FromJSON RealmDto where
    parseJSON (Object v) = RealmDto <$>
        v .: "cdn"<*>
        v .: "css"<*>
        v .: "dd"<*>
        v .: "l"<*>
        v .: "lg"<*>
        v .: "n"<*>
        v .: "profileiconmax"<*>
        v .: "store"<*>
        v .: "v"
    parseJSON _ = mzero
data RuneListDto = RuneListDto {
    _basic :: BasicDataDto,
    _data :: Map[string,,
    _type :: String,
    _version :: String
} deriving (Eq, Show)

makeLenses ''RuneListDto

instance FromJSON RuneListDto where
    parseJSON (Object v) = RuneListDto <$>
        v .: "basic"<*>
        v .: "data"<*>
        v .: "type"<*>
        v .: "version"
    parseJSON _ = mzero
data RuneDto = RuneDto {
    _colloq :: String,
    _consumeOnFull :: Bool,
    _consumed :: Bool,
    _depth :: Int,
    _description :: String,
    _from :: [String],
    _gold :: GoldDto,
    _group :: String,
    _hideFromAll :: Bool,
    _id :: Int,
    _image :: ImageDto,
    _inStore :: Bool,
    _into :: [String],
    _maps :: Map[string,,
    _name :: String,
    _plaintext :: String,
    _requiredChampion :: String,
    _rune :: MetaDataDto,
    _sanitizedDescription :: String,
    _specialRecipe :: Int,
    _stacks :: Int,
    _stats :: BasicDataStatsDto,
    _tags :: [String]
} deriving (Eq, Show)

makeLenses ''RuneDto

instance FromJSON RuneDto where
    parseJSON (Object v) = RuneDto <$>
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
data SummonerSpellListDto = SummonerSpellListDto {
    _data :: Map[string,,
    _type :: String,
    _version :: String
} deriving (Eq, Show)

makeLenses ''SummonerSpellListDto

instance FromJSON SummonerSpellListDto where
    parseJSON (Object v) = SummonerSpellListDto <$>
        v .: "data"<*>
        v .: "type"<*>
        v .: "version"
    parseJSON _ = mzero
data SummonerSpellDto = SummonerSpellDto {
    _cooldown :: [double],
    _cooldownBurn :: String,
    _cost :: [Int],
    _costBurn :: String,
    _costType :: String,
    _description :: String,
    _effect :: [object],
    _effectBurn :: [String],
    _id :: Int,
    _image :: ImageDto,
    _key :: String,
    _leveltip :: LevelTipDto,
    _maxrank :: Int,
    _modes :: [String],
    _name :: String,
    _range :: object,
    _rangeBurn :: String,
    _resource :: String,
    _sanitizedDescription :: String,
    _sanitizedTooltip :: String,
    _summonerLevel :: Int,
    _tooltip :: String,
    _vars :: [SpellVarsDto]
} deriving (Eq, Show)

makeLenses ''SummonerSpellDto

instance FromJSON SummonerSpellDto where
    parseJSON (Object v) = SummonerSpellDto <$>
        v .: "cooldown"<*>
        v .: "cooldownBurn"<*>
        v .: "cost"<*>
        v .: "costBurn"<*>
        v .: "costType"<*>
        v .: "description"<*>
        v .: "effect"<*>
        v .: "effectBurn"<*>
        v .: "id"<*>
        v .: "image"<*>
        v .: "key"<*>
        v .: "leveltip"<*>
        v .: "maxrank"<*>
        v .: "modes"<*>
        v .: "name"<*>
        v .: "range"<*>
        v .: "rangeBurn"<*>
        v .: "resource"<*>
        v .: "sanitizedDescription"<*>
        v .: "sanitizedTooltip"<*>
        v .: "summonerLevel"<*>
        v .: "tooltip"<*>
        v .: "vars"
    parseJSON _ = mzero


