{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
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
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
import Data.Maybe ( mapMaybe )
-- ****** Data.Foldable ( foldl' )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', elemIndex
-- ****** Data.List.Extra: splitOn
import Data.List.Extra ( splitOn )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import qualified Control.Monad.Trans.State as St
-- import qualified Data.Bits as Bits

import System.Environment (getArgs)
import Data.Char (digitToInt)

-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug :: c -> String -> c
-- debug = flip trace

-- import qualified Text.Megaparsec            as P
-- import qualified Text.Megaparsec.Char       as P
-- import qualified Text.Megaparsec.Char.Lexer as PP
-- import Data.Void (Void)

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2021 - Day 02 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input25/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let allR = parseRanges content
  putStrLn $ "allR=" ++ show allR

  let invalids = mapMaybe searchInvalid allR
  putStrLn $ "invalids=" ++ show invalids

  -- let stepSearch (r, h, l) = do
  --       putStrLn $ "search for " ++ show r ++ " (" ++ show l ++ ", " ++ show h ++ " )"
  --       let res = searchInvalid r l h
  --       putStrLn $ "  res=" ++ show res
  -- mapM_ (\(r,l,h) -> do
  --           putStrLn $ "search for " ++ show r ++ " (" ++ show l ++ ", " ++ show h ++ " )"
  --           let res = invalidInRange r l h
  --           putStrLn $ "  res=" ++ show res
  --       ) invalids

  -- let pRes = (sum . concat) invalids
  -- putStrLn $ "Answer 1> " ++ show pRes

  -- mapM_ (\r -> do
  --           putStrLn $ "base for " ++ show r
  --           let res = commonBase r
  --           putStrLn $ "  res=" ++ show res
  --           let (base, h, l) = res
  --           let mc = potPeriods base
  --           putStrLn $ "mCycle=" ++ show mc
  --           let iv = listInvalids r
  --           putStrLn $ "iv=" ++ show iv
  --           -- let stillBad = case iv of
  --           --       Just ll -> head ll
  --           --       Nothing -> -3
  --           -- if stillBad == (-1) || stillBad < (-10) then putStrLn "****************\n" else return()
  --       ) allR
  let newInvalids = Set.toList (Set.fromList $ concatMap listInvalids allR)
  -- putStrLn $ "newInvalids" ++ show newInvalids
  let cRes = sum newInvalids
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
data Range = R { sLow :: String
               , sHigh :: String
               }
  deriving (Show)

parseRanges line = map makeRange ranges
  where
    ranges = splitOn "," $ (reverse . drop 1 . reverse) line
    makeRange str = R l h
      where (l:h:_) = splitOn "-" str

searchInvalid range
  | odd (length (sLow range)) && length (sLow range) == length (sHigh range) = Nothing
  -- | even (length (sLow range)) = Just (semiLow, semiHigh)
  -- | even (length (sHigh range)) = Just (semiLow, semiHigh)
  -- | otherwise = Just (range, semiLow, semiHigh)
  | otherwise = Just $ invalidInRange range semiLow semiHigh
  where
    sizeHigh = length (sHigh range) - length (sHigh range) `div` 2
    (semiHigh, restHigh) = splitAt sizeHigh (sHigh range)
    semiLow = take (length (sLow range) - length restHigh) (sLow range)

invalidInRange :: Range -> String -> String -> [Int]
invalidInRange rng "" limitHigh = invalidInRange rng "0" limitHigh
invalidInRange rng limitLow limitHigh = mapMaybe (opCheck rLow rHigh . concatInt) [low .. high]
  where
    rLow = read (sLow rng)
    rHigh = read (sHigh rng)
    low = read limitLow
    high = read limitHigh
    opCheck vl vh val = if vl <= val && val <= vh then Just val
                                                  else Nothing

concatInt :: Int -> Int
concatInt val = read (show val ++ show val)
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- listInvalids range = case minCycle base of
--   Just period ->
--   where
--     (base, iLow, iHigh) = commonBase range
-- listInvalids range
--   -- can enumerate
--   | length (sHigh range) < 4 = Just $ filter isInRange ([i*10+i | i <- [1..9]] ++
--                                                  [i*100+i*10+i | i <- [1..9]])
--   | length (sLow range) == 3 && length (sHigh range) == 4 =
--     Just (filter isInRange ([i*100+i*10+i | i <- [1..9]])
--     ++ filter isInRange ([i*1000+j*100+i*10+j | i <- [1..9], j <- [0..9]]))
--   | length (sLow range) == 4 && length (sHigh range) == 4 =
--     Just (filter isInRange ([i*1000+j*100+i*10+j | i <- [1..9], j <- [0..9]]))
--   | length (sLow range) == 6 && length (sHigh range) == 6 =
--     Just (filter isInRange ([k*100000+i*10000+j*1000+k*100+i*10+j | k <- [1..9], i <- [0..9], j <- [0..9]]))
--   | length (sLow range) == length (sHigh range) && length (sLow range) == 5 = listInvalidPrime range 5
--   | length (sLow range) == length (sHigh range) && length (sLow range) == 7 = listInvalidPrime range 7
--   | not (null base) = Just $ isBiggerHalf : Set.toList (Set.fromList $ mapMaybe (fmap read . genRepeat (length (sHigh range))) (potPeriods base))
--   | otherwise = Just [-1]
--   where
--     isinrange val = val >= read (slow range) && val <= read (shigh range)
--     (base, iLow, iHigh) = commonBase range
--     maxBase = last (potPeriods base)
--     isBiggerHalf = if length maxBase > (length (sHigh range) `div` 2) then (-4) else (- 10 + (length maxBase - (length (sHigh range) `div` 2)))
--     -- for number with prime number of sigits
--     listInvalidPrime range n = case length base of
--       0 -> Just $ filter isInRange ([read (replicate n c) | c <- ['1' .. '9']])
--       1 -> Just $ filter isInRange [read (replicate n (head base))]
--       _ -> if all (== head base) base then Just $ filter isInRange [read (replicate n (head base))]
--                                       else Nothing
    -- complement = case length maxBase
    --   where
    --     goodPeriods = filter (>= length maxBase) (dividers (sLow range))


-- find the "common base" between Low and High range
-- ex 4-19 => "" + [4-19], 153321 et 153478 => 153 + [2-4], 28-49 et "" + [2-4]
commonBase range
  | length (sLow range) == length (sHigh range) = (base, digitToInt lower, digitToInt higher)
  | otherwise = ("", read (take 1 (sLow range)), read (take 2 (sHigh range)))
  where
    base = map fst (takeWhile (\(l,h) -> l==h) $ zip (sLow range) (sHigh range))
    lower = (sLow range) !! length base
    higher = (sHigh range) !! length base


potPeriods str = periods -- if (length periods) == 0 then Nothing else Just (head periods)
  where
    periods = mapMaybe opCheckPeriod [1..(length str)]
    opCheckPeriod n = if isRepeating (take n str) str then Just (take n str) else Nothing

-- is pat repeating itself in str ?
isRepeating :: String -> String -> Bool
isRepeating pat str = all (isSame (head splitted)) splitted
  where
    splitted = splitNb (length pat) str
    isSame p1 p2 = p1 == p2 || (take (length p2) p1 == p2)

splitNb :: Int -> String -> [String]
splitNb _ "" = []
splitNb n str = take n str : splitNb n (drop n str)

-- try to generate a string of length 'n' by repeating pat
genRepeat :: Int -> String -> Maybe String
genRepeat n pat
  | mod n (length pat) == 0 = Just $ concat (replicate (div n (length pat)) pat)
  | otherwise = Nothing

dividers str = filter (\d -> mod len d == 0) [1..(len-1)]
  where
    len = length str

-- -- can complete ONLY a maximal period with up to (length sHigh) / 2
-- allComplement :: Int -> Int -> Int -> [String]
-- allComplement (-1) _ _ = [""]
-- allComplement nextSize firstLow firstHigh = do
--   c <- [firstLow .. firstHigh]
--   cs <- allComplement (nextSize-1) 0 9
--   return (show c ++ cs)

-- very SMART: if a number repeats (every 3 for example), then it is divided by 001001001
-- et donc diviser par tous les potentiel pattern, ce qui dépend des périodes possibles
makeDivider :: Int -> Int -> Int
makeDivider len pSize = read $ take len (concat (repeat (replicate (pSize-1) '0' ++ "1")))

-- suppose both iLow and iHigh have the same "str size"
-- invalidsWithinLimits strLow strHigh =
--   where
--     divs = dividers strLow

invalidWithPeriod strLow strHigh sizePeriod = candidates -- (perLow, perHigh, sizePeriod, pat)
  where
    iLow = read strLow :: Int
    iHigh = read strHigh :: Int
    pat = makeDivider (length strLow) sizePeriod
    perLow = div iLow pat
    perHigh = 1 + div iHigh pat
    candidates = filter (isInRange iLow iHigh) $  map (*pat) [perLow..perHigh]

listInvalids :: Range -> [Int]
listInvalids range
  | length (sLow range) == length (sHigh range) =
    concatMap (invalidWithPeriod (sLow range) (sHigh range)) (dividers (sLow range))
  | otherwise = concatMap (invalidWithPeriod (sLow range) maxLow) (dividers (sLow range))
                ++ concatMap (invalidWithPeriod minHigh (sHigh range)) (dividers minHigh)
    where
      maxLow = replicate (length (sLow range)) '9'
      minHigh = '1': replicate (length (sHigh range) - 1) '0'

isInRange imin imax val = val >= imin && val <= imax
