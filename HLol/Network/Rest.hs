module HLol.Network.Rest (
    get,
    sendAPIRequest,
    sendAPIRequest',
    LolRequest,
    LolError,
    Region(..)
    ) where

import HLol.Utils

import Network.Curl

import Data.Aeson
import Data.List (intercalate)
import Data.ByteString.Lazy (ByteString)

data LolError =
    BadRequest |
    Unauthorized |
    StatsDataNotFound |
    RateLimitExceeded |
    InternalServerError |
    ServiceUnavailable
    deriving (Show, Eq)

data Region = EUW | NA

instance (Show Region) where
    show EUW    = "euw"
    show NA     = "na"

api_key :: String
api_key = "0e27e5ee-0a34-4e08-abb3-7f8186b4f6d4"

base_url :: Region -> String
base_url r = let rs = show r in "https://" ++ rs ++ ".api.pvp.net/api/lol/" ++ rs

type LolRequest = String

get :: (FromJSON a) => String -> IO (Either LolError a)
get url = do
    resp <- sendAPIRequest url []
    return $ mapR (getRight . eitherDecode) resp

sendAPIRequest :: LolRequest -> [(String, String)] -> IO (Either LolError ByteString)
sendAPIRequest url opts = do
    let opts_str = intercalate "&" . map (\(x, y) -> x ++ "=" ++ y) $ ("api_key", api_key) : opts
    let url_str = base_url EUW ++ url ++ "?" ++ opts_str
    resp <- curlGetResponse_ url_str [] :: IO (CurlResponse_ [(String, String)] ByteString)
    case respCurlCode resp of
        CurlOK  ->  return $ Right $ respBody resp
        _       ->  case respStatus resp of
                        400 -> error $ show BadRequest
                        401 -> error $ show Unauthorized
                        404 -> error $ show StatsDataNotFound
                        429 -> error $ show RateLimitExceeded
                        500 -> error $ show InternalServerError
                        503 -> error $ show ServiceUnavailable
                        e   -> error $ "Unknown error code: " ++ show e

sendAPIRequest' :: LolRequest -> IO (Either LolError ByteString)
sendAPIRequest' url_str = do
    resp <- curlGetResponse_ url_str [] :: IO (CurlResponse_ [(String, String)] ByteString)
    case respCurlCode resp of
        CurlOK  ->  return $ Right $ respBody resp
        _       ->  case respStatus resp of
                        400 -> error $ show BadRequest
                        401 -> error $ show Unauthorized
                        404 -> error $ show StatsDataNotFound
                        429 -> error $ show RateLimitExceeded
                        500 -> error $ show InternalServerError
                        503 -> error $ show ServiceUnavailable
                        e   -> error $ "Unknown error code: " ++ show e
