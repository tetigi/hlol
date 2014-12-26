module Grouch.Logging.Logger (
     withLogging
    ,lolInfo
    ,lolError
    ,lolDebug
    ) where

import Data.Time
import Data.Time.ISO8601
import Text.Printf

import System.IO
import System.Log.Logger
import System.Log.Handler.Simple

logFile :: FilePath
logFile = "hlol.log"

withLogging :: IO a -> IO a
withLogging action = do
    handle <- openFile logFile AppendMode
    s <- verboseStreamHandler handle INFO
    updateGlobalLogger rootLoggerName (addHandler s)
    updateGlobalLogger rootLoggerName (setLevel INFO)
    result <- action
    hClose handle
    return result

getNiceTime :: IO String
getNiceTime = do
    time <- getCurrentTime
    return $ formatISO8601 time

assembleLogMessage :: String -> IO String
assembleLogMessage msg = do
    timeStr <- getNiceTime
    return $ printf "[%s] %s" timeStr msg

lolInfo :: String -> String -> IO ()
lolInfo logger msg = infoM logger =<< assembleLogMessage msg

lolError :: String -> String -> IO ()
lolError logger msg = errorM logger =<< assembleLogMessage msg

lolDebug :: String -> String -> IO ()
lolDebug logger msg = debugM logger =<< assembleLogMessage msg
