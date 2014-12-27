module HLol.Network.Rest (
    sendAPIRequest,
    LolRequest
    ) where

import Network.Curl ( curlGetString_ )
import Network.Curl.Code ( CurlCode( CurlOK ) )

import Data.List (intercalate)
import Data.ByteString.Lazy (ByteString)

data Region = EUW | NA

instance (Show Region) where
    show EUW    = "euw"
    show NA     = "na"

api_key :: String
api_key = "0e27e5ee-0a34-4e08-abb3-7f8186b4f6d4"

base_url :: Region -> String
base_url r = let rs = show r in "https://" ++ rs ++ ".api.pvp.net/api/lol/" ++ rs

type LolRequest = String

sendAPIRequest :: LolRequest -> [(String, String)] -> IO ByteString
sendAPIRequest url opts = do
    let opts_str = intercalate "&" . map (\(x, y) -> x ++ "=" ++ y) $ ("api_key", api_key) : opts
    let url_str = base_url EUW ++ url ++ "?" ++ opts_str
    (code, output) <- curlGetString_ url_str []
    case code of
        CurlOK  -> return output
        _       -> error $ "Error: Could not get services. Got response: " ++ show code ++ "\n" ++ show output
