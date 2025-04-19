module MyUtils where

import Data.List (groupBy)

-- *****************************************************************************
-- countTrue: count nb of element filtered in list
-- *****************************************************************************
countTrue :: (a -> Bool) -> [a] -> Int
countTrue p = length . filter p

-- *****************************************************************************
-- groupLines from a list of String with empty lines, group them.
-- *****************************************************************************
groupLines :: [String] -> [[String]]
groupLines = filter (/= [""]) . groupBy (\x y -> x /= "" && y /= "")
