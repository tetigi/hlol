module HLol.Network.Rest (
    get,
    getWithOpts,
    sendAPIRequest,
    sendAPIRequest',
    sendAPIRequest_,
    LolRequest,
    LolError,
    Region(..)
    ) where

import HLol.Data.Common
import HLol.Logging.Logger
import HLol.Utils

import Control.Monad
import Network.Curl

import Data.Aeson
import Data.List (intercalate)
import Data.ByteString.Lazy (ByteString)

tag :: String
tag = "HLol.Network.Rest"

api_key :: String
api_key = "0e27e5ee-0a34-4e08-abb3-7f8186b4f6d4"

base_url :: Region -> String
base_url r = let rs = show r in "https://" ++ rs ++ ".api.pvp.net/api/lol/" ++ rs

type LolRequest = String

get :: (FromJSON a) => String -> IO (Either LolError a)
get url = do
    resp <- sendAPIRequest url []
    return $ join $ mapR (liftError . eitherDecode) resp

getWithOpts :: (FromJSON a) => String -> [(String, String)] -> IO (Either LolError a)
getWithOpts url opts = do
    resp <- sendAPIRequest url opts
    return $ join $ mapR (liftError . eitherDecode) resp

sendAPIRequest' :: (Region -> String) -> LolRequest -> [(String, String)] -> IO (Either LolError ByteString)
sendAPIRequest' base_urlf url opts = do
    let opts_str = intercalate "&" . map (\(x, y) -> x ++ "=" ++ y) $ ("api_key", api_key) : opts
    let url_str = base_urlf EUW ++ url ++ "?" ++ opts_str
    lolInfo tag $ "GET: " ++ url_str
    resp <- curlGetResponse_ url_str [] :: IO (CurlResponse_ [(String, String)] ByteString)
    case respCurlCode resp of
        CurlOK  ->  return $ Right $ respBody resp
        _       ->  do
                        lolError tag $ "GET \"" ++ url_str ++ "\" gave response: " ++ show code
                        return $ Left code
                    where code = getErrorCode $ respStatus resp

sendAPIRequest :: LolRequest -> [(String, String)] -> IO (Either LolError ByteString)
sendAPIRequest = sendAPIRequest' base_url

sendAPIRequest_ :: LolRequest -> IO (Either LolError ByteString)
sendAPIRequest_ url_str = do
    resp <- curlGetResponse_ url_str [] :: IO (CurlResponse_ [(String, String)] ByteString)
    lolInfo tag $ "GET: " ++ url_str
    case respCurlCode resp of
        CurlOK  ->  return $ Right $ respBody resp
        _       ->  do
                        lolError tag $ "GET \"" ++ url_str ++ "\" gave response: " ++ show code
                        return $ Left code
                    where code = getErrorCode $ respStatus resp

getErrorCode :: Int -> LolError
getErrorCode i =
    case i of
        400 -> BadRequest
        401 -> Unauthorized
        404 -> NotFound
        429 -> RateLimitExceeded
        500 -> InternalServerError
        503 -> ServiceUnavailable
        e   -> error $ "Unknown error code: " ++ show e
