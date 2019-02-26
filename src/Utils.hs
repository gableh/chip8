module Utils where

import Numeric

fromHex :: (Eq a, Num a) => String -> a
fromHex n = (fst (readHex n !! 0))