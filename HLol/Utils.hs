module HLol.Utils where

save :: (Read a, Show a) => FilePath -> a -> IO ()
save path item = writeFile path $ show item

load :: (Read a, Show a) => FilePath -> IO a
load = fmap read . readFile
