module Main where

import HLol.API.Summoner

main :: IO ()
main = do
    putStrLn "Hello, world!"
    names <- getByNames ["tetigi"]
    print names
