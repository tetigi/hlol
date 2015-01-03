module HLol.Utils where

import HLol.Data.Common

liftError :: Either String a -> Either LolError a
liftError (Left e) = Left $ ParseError e
liftError (Right r) = (Right r)

mapR :: (b -> c) -> Either a b -> Either a c
mapR f (Right x)    = Right $ f x
mapR _ (Left y)     = Left y

getRight :: (Show a) => Either a b -> b
getRight (Right x) = x
getRight (Left y) = error $ show y

save :: (Read a, Show a) => FilePath -> a -> IO ()
save path item = writeFile path $ show item

load :: (Read a, Show a) => FilePath -> IO a
load = fmap read . readFile
