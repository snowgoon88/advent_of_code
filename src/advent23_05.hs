module Main where

import qualified MyParser as MP
import Data.Char ( digitToInt, isDigit )

startFile = "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15"
exSeeds = MP.parseLabelList "seeds" (head (lines startFile))
exTables = reverse (parseMappers (drop 2 (lines startFile)) emptyMapper [])

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 05 - Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input05.txt"
  -- content <- readFile "Input23/test05_1.txt"

  let seeds = MP.parseLabelList "seeds" (head (lines content))
  let tables = reverse (parseMappers (drop 2 (lines content)) emptyMapper [])
  -- putStrLn $ show tables
  let converted = map (convert tables) (MP.numList seeds)
  print converted
  let pRes = minimum converted --foldr min 0 converted

  putStrLn $ "Answer 1> " ++ show pRes

  let allSeeds = extends (reverse (MP.numList seeds)) []
  -- print allSeeds
  let someSeeds = extends [3127166940, 10916047] []

  let splitedSeed = splitS (reverse (MP.numList seeds)) []
        where splitS [] splitted = splitted
              splitS (s:r:ns) splitted = splitS ns ([s, r]:splitted)
  print splitedSeed

  -- split the seeds in sub intervalles
  print "The minimum is one of:"
  mapM_ (\sp -> print ( minimum ( map (convert tables) (extends sp [])) )) splitedSeed

  -- WARN : Brute force is not working, i.e: return the last minimum because of memory pbs ?
  -- let cRes = minimum (map (convert tables) allSeeds)

  -- putStrLn $ "Answer 2> " ++ show cRes
  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

type MapTable = [(Int, Int, Int)]
mapper = [(50, 98, 2), (52, 50, 48)]

translate :: Int -> MapTable -> Int
translate x [] = x
translate x ((dest, src, range):ms)
  | (x - src) >= 0 && (x - src) < range = dest + (x - src)
  | otherwise = translate x ms

data Mapper = Mapper { nameM :: String
                     , mapperM :: MapTable }
  deriving Show
emptyMapper = Mapper { nameM = "undefined", mapperM = [] }

-- parseMappers :: linesToRead curMapper listMapper
parseMappers :: [String] -> Mapper -> [Mapper] -> [Mapper]
-- empty line (or []) : add curMapper
parseMappers [] curMapper listMapper = curMapper:listMapper
parseMappers ("":ls) curMapper listMapper = parseMappers ls emptyMapper (curMapper:listMapper)
-- line begins with digit -> new MapTable
parseMappers (l:ls) curMapper listMapper
  | isDigit (head l) = parseMappers ls curMapper { mapperM = updatedMapper } listMapper
  | otherwise = parseMappers ls curMapper { nameM = l} listMapper
    where digits = MP.numList (MP.parseList MP.emptyListInfo l)
          newTable = (digits!!2, digits!!1, head digits)
          updatedMapper = newTable:(mapperM curMapper)

-- map a seed, using foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
convert mapTables x = foldl translate x (map mapperM mapTables)

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- BRUTE FORCE : something is wrong...
-- extends seeds using range
extends :: [Int] -> [Int] -> [Int]
extends [] digits = digits
extends (start:range:ns) digits = take range [start..] ++ digits

-- NOT NEEDED AFTER ALL --------------------------------------------------------
-- -- Working on Intervals
-- type Interval = (Int, Int) -- (start, range)

-- -- project mapTable interval listInterval-> [inteval]
-- project ((dest, src, range):ms) (is,ir) listInterval
--   -- (is, ir) contained in Mapper
--   | src <= is && ir <= range = (translate is [(dest, src, range)], ir):listInterval
--   | src <= is = project (is, range)
