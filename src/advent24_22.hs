{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks, getValMap)
-- import qualified MyCache as MC

-- import Data.String.Utils ( split, join, startswith) --, startswith, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- import Data.Maybe ( catMaybes, fromMaybe ) -- fromJust, fromMaybe, catMaybes, isNothing )
import qualified Data.Map as Map
-- import qualified Data.Set as Set
import Data.List ( sort, group, find, delete, sortOn, groupBy ) -- delete, sortOn
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import Control.Monad.State
import qualified Data.Bits as Bits
-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug = flip trace


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2024 - Day 22 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input22.txt"
  -- content <- readFile "Input24/test22_1.txt"

  let (next10, cache10) = foldl opMapSecret ([123], Map.empty) [0..9]
  -- print $ "(next10, cache10)=" ++ show (next10, cache10)
  -- print $ "next10=" ++ show (reverse next10)

  let (s2000_1, c2000_1) = foldl opNext (1, cache10) [0..1999]
  -- print $ "s2000_1=" ++ show s2000_1

  let (s2000_10, c2000_10) = foldl opNext (10, c2000_1) [0..1999]
  -- print $ "s2000_10=" ++ show s2000_10

  let (s2000_100, c2000_100) = foldl opNext (100, c2000_10) [0..1999]
  -- print $ "s2000_100=" ++ show s2000_100

  let (s2000_2024, c2000_2024) = foldl opNext (2024, c2000_100) [0..1999]
  -- print $ "s2000_2024=" ++ show s2000_2024

  print "==== Using Fold"
  let (res, foldCache) = foldl (opStep 2000) ([], c2000_2024) (map read (lines content))
  -- print $ "res=" ++ show res
  -- print $ "sum res=" ++ show (sum res)
  let pRes = sum res
  putStrLn $ "Answer 1> " ++ show pRes

  -- DEBUG

  let listSecret = map read (lines content) :: [Int]
  -- print $ "listSecret=" ++ show listSecret
  let (comb, pMap, cMap) = findSequence foldCache listSecret
  print $ "comb=" ++ show comb

  --print $ "lDebug" ++ show (Map.lookup (-2,1,-3,3) pMap)
  let cRes = snd comb
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type CacheMap = Map.Map Int Int

prune :: Int -> Int
prune secret = mod secret 16777216

mix :: Int -> Int -> Int
mix secret n = Bits.xor secret n

mulMixPrune :: Int -> Int
mulMixPrune secret = prune $ mix (secret * 64) secret

divMixPrune :: Int -> Int
divMixPrune secret = prune $ mix (div secret 32) secret

mulFinal :: Int -> Int
mulFinal secret = prune $ mix (secret * 2048) secret

secretStep :: Int -> Int
secretStep secret = mulFinal (divMixPrune (mulMixPrune secret))

cachedStep :: Int -> CacheMap -> (Int, CacheMap)
cachedStep secret cache = case Map.lookup secret cache of
  Just next -> (next, cache)
  Nothing -> (next, newCache)
    where
      next = secretStep secret
      newCache = Map.insert secret next cache

opMapSecret :: ([Int], CacheMap) -> Int -> ([Int], CacheMap)
opMapSecret (acc,cache) dummy = (next:acc, newCache)
  where (next, newCache) = cachedStep (head acc) cache

opNext :: (Int, CacheMap) -> Int -> (Int, CacheMap)
opNext (n, cache) dummy = cachedStep n cache

opStep :: Int -> ([Int], CacheMap) -> Int -> ([Int], CacheMap)
opStep nbStep (acc, cache) secret = (stepped:acc, newCache)
  where (stepped, newCache) = foldl opNext (secret, cache) [0.. nbStep-1]
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- parsing list we will store prices in Map.Map (Int, Int, Int, Int) -> (Int)
-- checking with another map it is the first time the combination is met
type Combination = (Int, Int, Int, Int)
type PriceMap = Map.Map Combination Int
type CheckMap = Map.Map Combination Bool

ltest :: [Int]
ltest = [123, 15887950,16495136, 527345, 704524, 1553684, 12683156
        ,11100544, 12249484, 7753432, 5908254]

moduloDiff :: [Int] -> [(Int, Int)]
moduloDiff ns = zip (diffList $ modmap) (drop 1 modmap)
  where
    modmap = map (`mod` 10 ) ns
    diffList :: [Int] -> [Int]
    diffList [n1, n2] = [n2 - n1]
    diffList (n1:n2:ns) = (n2-n1): diffList (n2:ns)

parseBuyer :: [(Int,Int)] -> PriceMap -> CheckMap -> PriceMap
parseBuyer [p1, p2, p3] priceMap checkMap = priceMap
parseBuyer ((c1,p1):(c2,p2):(c3,p3):(c4,p4):ps) priceMap checkMap = case Map.lookup (c1,c2,c3,c4) checkMap of
  -- already seen in this Buyer
  Just True -> parseBuyer ((c2,p2):(c3,p3):(c4,p4):ps) priceMap checkMap
  Nothing -> parseBuyer ((c2,p2):(c3,p3):(c4,p4):ps) newPrice newCheck
  where
    prevPrice = Map.findWithDefault 0 (c1,c2,c3,c4) priceMap
    newPrice = Map.insert (c1,c2,c3,c4) (p4 + prevPrice) priceMap
    newCheck = Map.insert (c1,c2,c3,c4) True checkMap

findMax :: PriceMap -> (Combination, Int)
findMax priceMap = last (sortOn snd (Map.toList priceMap))

opParseBuyer :: PriceMap -> [Int] -> PriceMap
opParseBuyer priceMap buyerList = parseBuyer priceList priceMap Map.empty
  where
    priceList = moduloDiff buyerList

opMapBuyer :: ([[Int]], CacheMap) -> Int -> ([[Int]], CacheMap)
opMapBuyer (acc, cacheMap) secret = ((reverse next2000):acc, newCache)
  where (next2000, newCache) = foldl opMapSecret ([secret], cacheMap) [0..1999]

findSequence :: CacheMap -> [Int] -> ((Combination, Int), PriceMap, CacheMap)
findSequence cacheMap secretList = (findMax priceMap, priceMap, newCache)
  where
    (listBuyer, newCache) = foldl opMapBuyer ([], cacheMap) secretList
    priceMap = foldl opParseBuyer Map.empty listBuyer

-- Debug
findCombo :: Eq a => [[(a, b)]] -> (a, a, a, a) -> [(a, b)] -> [[(a, b)]]
findCombo acc _ [p1,p2,p3] = acc
findCombo acc (f1,f2,f3,f4) ((c1,p1):(c2,p2):(c3,p3):(c4,p4):ps)
  | (f1,f2,f3,f4) == (c1,c2,c3,c4) = findCombo ([(c1,p1),(c2,p2),(c3,p3),(c4,p4)]:acc) (f1,f2,f3,f4)
                                               ((c2,p2):(c3,p3):(c4,p4):ps)
  | otherwise = findCombo acc (f1,f2,f3,f4) ((c2,p2):(c3,p3):(c4,p4):ps)
