{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore,
--                readGrid, addDir, isValidPos, chunks, showGrid, getValMap, allDir)
-- import qualified MyCache as MC
-- ****** MyUtils: countTrue, groupLines

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
import Data.String.Utils (split)
import Text.Read (readMaybe)
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
import Data.Maybe ( mapMaybe)
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl'
import Data.List (sortOn)
-- ****** Data.List.Extra: splitOn
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import qualified Control.Monad.Trans.State as St
-- import qualified Data.Bits as Bits
-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug = flip trace


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2020 - Day 13 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input13.txt"
  -- content <- readFile "Input20/test13_1.txt"

  let (time, buses) = parsePb (lines content)
  putStrLn $ "Pb=" ++ show (time, buses)

  let wait = map (\b -> (b, waitTime time b)) buses
  putStrLn $ "Wait=" ++ show wait

  let pRes = solve1 wait
  putStrLn $ "Answer 1> " ++ show pRes

  let (time, busLines) = parsePbWithIndex (lines content)
  -- putStrLn $ "Pb=" ++ show (time, busLines)
  -- let pro = product (map fst busLines)
  -- putStrLn $ "pro=" ++ show (pro - 100000000000000)
  let sortedBus = reverse $ sortOn fst busLines
  -- putStrLn $ "Sorted=" ++ show sortedBus
  -- let mu1 = take 10 $ multipliers 1000 59 19 (4 - 7)
  -- putStrLn $ "mu1=" ++ show mu1
  -- let partial = solve2_bis 100000 sortedBus
  -- putStrLn $ "partial=" ++ show partial
  let sol2 = solve2_bis 100000000000000 sortedBus
  putStrLn $ "sol2=" ++ show sol2
  -- let mu1 = take 10 $ multipliers 100000000000000 821 463 (19 - 50)
  -- putStrLn $ "mu1=" ++ show mu1

  -- let sol2 = solve2 100000000000000 100000000 (sortedBus)
  -- let cRes = head (Set.toList sol2) - (snd (head sortedBus))
  -- putStrLn $ "Answer 2> " ++ show cRes

  -- SMART
  let s2 = soluce2 busLines
  putStrLn $ "s2=" ++ show s2

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
parsePb :: [String] -> (Int, [Int])
parsePb strs = (time, buses)
  where
    time = read $ head strs
    buses = mapMaybe readMaybe $ split "," (head (drop 1 strs))

waitTime :: Integral a => a -> a -> a
waitTime time bus = bus - (mod time bus)

solve1 :: (Num a, Ord a) => [(a, a)] -> a
solve1 waitTimes = bid * w
  where
    (bid, w) = head $ sortOn snd waitTimes
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

{--
t = b0 * kO         => k0 = t / b0
t = b1 * k1 + 1     => k1 = (t-1) / k1 = (b0 * k0 - 1) / k1

t = (b0 + b01) * k1 + 1  = b0 * k1 + (b1 - b0) * k1 + 1



t = bi + ki + i     => ki = (t-i) / ki
--}

-- TAKEN FROM
-- https://byorgey.wordpress.com/2020/02/07/competitive-programming-in-haskell-primes-and-factoring/

-- infinite list
primes :: [Integer]
primes = 2 : sieve primes [3..]
  where
    sieve (p:ps) xs =
      let (h,t) = span (< p*p) xs
      in  h ++ sieve ps (filter ((/=0).(`mod`p)) t)

-- list factors of a number
listFactors :: Integer -> [Integer]
listFactors = go primes
  where
    go _      1 = []
    go (p:ps) n
      | p*p > n = [n]
      | n `mod` p == 0 = p : go (p:ps) (n `div` p)
      | otherwise      = go ps n

-- | parsePbWithIndex => (timstamp, [(busId, line)])
parsePbWithIndex :: [String] -> (Int, [(Int, Int)])
parsePbWithIndex strs = (time, buses)
  where
    time = read $ head strs
    buses = mapMaybe readBus $ zip (split "," (head (drop 1 strs))) [0..]
      where readBus (s, idx) = case readMaybe s of
              Nothing -> Nothing
              Just val -> Just (val, idx)

-- look for k1 * 59  + 2 = k2 * 31
-- 59 = 31 + 28
-- k.59 = k.31 + k.28 => k.28 = a.31 - 1
-- => (a, a.31-1) head (filter (\y -> (mod (snd y) 28) == 0) [(x, 31*x-1) | x <- [1..]]
-- k = a.31-1/28, pour tous les a, a+28, a+2*28

-- 2.31 = 59 + 3
-- 2k.31 = k.59 + k.3 + 1

-- 59 = 3.19 + 2
-- k.59 = 2k.19 + k.2 => k.2 = a.19 - 3

-- 59 = 4.13 + 7
-- k.59 = 4k.13 + k.7 => k.7 = a.13 + 3

-- 59 = 8.7 + 3
-- k.59 = 8k.7 + K.3 => k.3 = a.7 + 4

-- | generate all possible (k*big) where k*big = k1*small + diff
--   (diff is positif if small is left of big)
-- multipliers :: Integral a => a -> a -> a  -> a -> [(a, a, a)]
-- multipliers thres big small diff = map (\(a, b) -> ((div b r) * big, a, b)) allK
--   where
--     -- (small.x - diff) / r * big > thres
--     -- x > (thres / big * r - diff) / small
--      -- (div b r) must be bigger than (div thres big) = minKbig
--     -- so small*x + diff > (minKBig / r) => (x > (minBKBig / r) - diff) / small
--     (m, r) = divMod big small
--     minKX = div (((div thres big) * r) - diff) small
--     allK = filter (\y -> (mod (snd y) r) == 0) [(x, small*x+diff) | x <- [minKX..]]

-- | coefGen: give the first coef k (and increment) such that busLines are synchronized
-- for biggest and secondBiggest AFTER thres
coefGen :: Integral a => a -> a -> a -> a -> (a, a, a -> a)
coefGen thres big small diff = (first, r, \k -> big * (div (small*k+diff) r))
  where
    (m, r) = divMod big small
    minKX = 1 + div ((((div thres big)+1) * r) - diff) small
    (first, _) = head $ filter (\y -> (mod (snd y) r) == 0) [(x, small*x+diff) | x <- [minKX..]]

-- test for all possible solutions using the big, second big generated synchronized
-- solutions
solve2_bis :: Integral a => a -> [(a, a)] -> a
solve2_bis thres busLSorted = head (filter (isSolution busLSorted) toCheck)
  where
     ((biggest, idxBiggest):(second, idxSecond):bs) = busLSorted
     (k, inc, fun_val) = coefGen thres biggest second (idxBiggest - idxSecond)
     -- we want (div (small.(k + a.inc)+diff) inc) * big > thres
     -- => a > div ((div (((div thres big) * inc) - diff) wmall) - k) inc
     -- minA = div ((div (((div thres biggest) * inc) - (idxBiggest - idxSecond)) second) - k) inc
     toCheck = map (\k -> fun_val k - idxBiggest) [k, k+inc ..]


-- NOT WORKING => too much memory usage
-- solve2 :: Integral a => a -> Int -> [(a, a)] -> Set.Set a
-- solve2 thres nbTaken busLSorted = foldl opFold setInit (drop 2 busLSorted)
--   where
--     ((biggest, idxBiggest):(second, idxSecond):bs) = busLSorted
--     lcompat = take nbTaken (multipliers thres biggest second (idxBiggest - idxSecond))
--     setInit = Set.fromList lcompat
--     opFold setCompat (busId, busIdx) = Set.intersection setCompat newSet
--       where
--         newSet = Set.fromList $ take nbTaken (multipliers thres biggest busId (idxBiggest - busIdx))

isSolution :: (Foldable t, Integral a) => t (a, a) -> a -> Bool
isSolution busLSorted val = all (onTime val) busLSorted
  where
    onTime val (id, line) = mod (val+line) id == 0

-- *****************************************************************************
-- *********************************************************************** SMART
-- *****************************************************************************

{--
First t mod 7 == 0 AND (t+1) mod 13 == 0 => 14 is solution, but also all 14 + (7*13).n

So repeat
new (id, coef) -> apply val += 7*13 until (val + coef) mod id == 0
                  then ready with newVal, oldInc*id
--}

soluce2 :: (Foldable t, Integral b) => t (b, b) -> (b, b)
soluce2 busLines = foldl syncBus (0, 1) busLines
  where
    -- until cond func a : repeat f(f(... a)) until condi
    syncBus (val, stepInc) (busId, busIdx) = (nextVal, stepInc * busId)
      where nextVal = until (\t -> mod (t+busIdx) busId == 0)
                            (+ stepInc)
                            val
