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
-- example applyN (+ 2) 10 1 |-> 21
applyN :: (a -> a) -> Int -> a -> a
applyN f n = \item -> iterate f item !! n

-- | Apply 'n' times a Monadic Action
-- example nTimesM monadAction 10 startValue
nTimesM :: (Ord t, Num t, Monad m) => (a -> m a) -> t -> a -> m a
nTimesM f n item
  | n > 0 = f item >>= nTimesM f (n-1)
  | otherwise = return item

-- | Apply a Monadic Action until a condition is True
-- example untilM isValidPath growPath startPath
-- see advent21_12.hs
untilM :: Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
untilM cond f item = do
  let finished = cond item
  if finished then return item
              else f item >>= untilM cond f
