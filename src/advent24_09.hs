{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks)
import Data.List ( sort ) -- sortOn, groupBy, find, group, sort, sortBy )
-- import Data.Tuple ( swap )
-- import Data.String.Utils ( split ) --, join, replace, split )
-- import qualified Data.Map.Strict as Map
import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
import Data.Maybe ( fromJust, catMaybes )
-- import Control.Monad ( fold )
import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import Data.List ( sortOn )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import Control.Monad.State
-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug = flip trace

l01 = "2333133121414131402"
b01 = "00...111...2...333.44.5555.6666.777.888899"
f01 = "0099811188827773336446555566.............."

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2024 - Day 09 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input09.txt"
  -- content <- readFile "Input24/test09_1.txt"

  -- let diskWithFree = concat (reverse (expandBlock [] 0 l01))
  let diskWithFree = concat (reverse (expandBlock [] 0 content))
  -- print $ "diskWithFree=" ++ show diskWithFree

  let idxDisk = zip [0..] diskWithFree
  -- print $ "idxDisk=" ++ show idxDisk
  let filled = fillDisk [] idxDisk (reverse idxDisk)
  -- print $ "filled" ++ show (reverse filled)

  let pRes = checksum filled 0
  putStrLn $ "Answer 1> " ++ show pRes

  -- let diskBlock = expandIntBlock [] Map.empty 0 0 l01
  let diskBlock = expandIntBlock [] Map.empty 0 0 content
  let (disk, fMap) = (fst diskBlock, Map.map sort (snd diskBlock))
  -- print $ "diskBlock=" ++ show diskBlock
  -- print $ "fMap=" ++ show fMap

  -- print $ "best5=" ++ show (findBlock fMap (40, 5, 9))
  -- print $ "best3=" ++ show (findBlock fMap (40, 3, 9))
  -- print $ "best2=" ++ show (findBlock fMap (40, 2, 22))
  -- print $ "best1=" ++ show (findBlock fMap (40, 1, 19))

  let newBlocks = rePack [] fMap disk
  -- print $ "newBlocks=" ++ show newBlocks

  let cRes = sum (map checksumBlock newBlocks)
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

expandBlock disk _ [] = disk
expandBlock disk id (b:bs) = expandFree  (replicate (digitToInt b) id:disk) (id+1) bs
expandFree disk _ [] = disk
expandFree  disk id (b:bs) = expandBlock (replicate (digitToInt b) (-1):disk) id bs

fillDisk :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
fillDisk after ((itD,d):ds) ((itE,e):es)
  | itD > itE = after
  | d >= 0      = fillDisk ((itD,d):after) ds ((itE,e):es)
  | otherwise  = fillDisk ((itD,p):after) ds ps
  where
    ((_,p):ps) = pass ((itE, e):es)
    pass :: [(Int, Int)] -> [(Int, Int)]
    pass ((it, i):is) = if i < 0 then pass is
                                  else (it, i):is

checksum :: [(Int, Int)] -> Int -> Int
checksum [] acc = acc
checksum ((it,id):d) acc
  | id >=0    = checksum d (acc + (it*id))
  | otherwise = checksum d acc
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
type Block = (Int, Int, Int) -- pos, len, id
type FreeMap = Map.Map Int [Int]

expandIntBlock :: [Block] -> FreeMap -> Int -> Int -> [Char] -> ([Block], FreeMap)
expandIntBlock disk freeMap _ _ [] = (disk, freeMap)
expandIntBlock disk freeMap id nextPos (b:bs) = expandIntFree ((nextPos, digitToInt b, id):disk) freeMap (id+1) (nextPos + (digitToInt b)) bs

expandIntFree :: [Block] -> FreeMap -> Int -> Int -> [Char] -> ([Block], FreeMap)
expandIntFree disk freeMap _ _ [] = (disk, freeMap)
expandIntFree  disk freeMap id nextPos (b:bs) = expandIntBlock disk newMap id (nextPos + lenFree) bs
  where
    lenFree :: Int
    lenFree = digitToInt b
    newMap :: FreeMap
    newMap = case Map.lookup lenFree freeMap of
      Just freeList -> Map.adjust (const (nextPos:freeList)) lenFree freeMap
      Nothing       -> Map.insert lenFree [nextPos] freeMap

-- find the first freeBlock
findBlock :: FreeMap -> Block -> Maybe (Int, Int, FreeMap)
findBlock freeMap (pos, len, id) = case sortedPos of
  (bestPos, bestLen):sp -> if bestPos < pos then Just (bestPos, bestLen, updateFreeMap (newMap bestLen) (bestPos, bestLen) len)
                                            else Nothing
  []                    -> Nothing
  where
    sortedPos = sort $ catMaybes (map (enoughSpace freeMap) [len .. 9])
    newMap lenToChange = Map.adjust (const newList) lenToChange freeMap
      where newList = drop 1 (freeMap Map.! lenToChange)

enoughSpace :: FreeMap -> Int -> Maybe (Int, Int)
enoughSpace freeMap len = case Map.lookup len freeMap of
  Just [] -> Nothing
  Just lPos -> Just (head lPos, len)
  Nothing -> Nothing

updateFreeMap :: FreeMap -> (Int, Int) -> Int -> FreeMap
updateFreeMap freeMap (posChanged, lenChanged) lenBlock
  | lenChanged == lenBlock = freeMap
  | otherwise = case Map.lookup diffLen freeMap of
      Nothing -> Map.insert diffLen [posChanged+lenBlock] freeMap
      Just listPos -> Map.insert diffLen (sort ((posChanged+lenBlock):listPos)) freeMap
      where diffLen = lenChanged - lenBlock

-- -- Create a new List of Blocks
rePack :: [Block] -> FreeMap -> [Block] -> [Block]
rePack acc _ [] = acc
rePack acc freeMap ((pos, len, id):bs) = case findBlock freeMap (pos, len, id) of
  Nothing -> rePack ((pos, len, id):acc) freeMap bs
  Just (bpos, blen, bFreeMap) -> rePack ((bpos, len, id):acc) bFreeMap bs

checksumBlock (pos, len, id) = sum $ map (* id) [pos .. pos+len-1]
