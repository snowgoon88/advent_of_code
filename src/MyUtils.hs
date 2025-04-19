module MyUtils where

-- *****************************************************************************
-- countTrue: count nb of element filtered in list
-- *****************************************************************************
countTrue :: (a -> Bool) -> [a] -> Int
countTrue p = length . filter p
