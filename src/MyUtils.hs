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

-- *****************************************************************************
-- For Enum and Bounded, circular pred and succ
-- *****************************************************************************
-- | Circular version of 'succ'
csucc :: (Eq a, Enum a, Bounded a) => a -> a
csucc x | x == maxBound = minBound
        | otherwise     = succ x

-- | Circular version of 'pred'
cpred :: (Eq a, Enum a, Bounded a) => a -> a
cpred x | x == minBound = maxBound
        | otherwise     = pred x

-- | Apply 'n' times a funtion
applyN :: (a -> a) -> Int -> a -> a
applyN f n = \item -> iterate f item !! n
