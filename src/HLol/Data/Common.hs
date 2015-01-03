module HLol.Data.Common where

data LolError =
    BadRequest |
    Unauthorized |
    NotFound |
    RateLimitExceeded |
    InternalServerError |
    ServiceUnavailable |
    ParseError String
    deriving (Show, Eq)

data Region = EUW | NA

instance (Show Region) where
    show EUW    = "euw"
    show NA     = "na"
