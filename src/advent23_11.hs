module Main where

-- import qualified MyParser as MP
-- import Data.List ( group, sort, sortOn, sortBy )
-- import Data.String.Utils ( replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust )

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 11 Part 1 & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input11.txt"
  -- content <- readFile "Input23/test11_1.txt"

  let positions = concat (map (\il -> parseLine [] (fst il) (zip (snd il) [0..]))
                          (zip [0..] (lines content)))
  print $ "pos=" ++ show positions

  let rows = map fst positions
  let cols = map snd positions
  let missingRows = missing rows
  let missingCols = missing cols
  print $ "miss_rows=" ++ show missingRows
  print $ "miss_cols=" ++ show missingCols
  let expandedRows = expand rows missingRows
  print $ "exp_rows=" ++ show expandedRows
  let expandedCols = expand cols missingCols
  print $ "exp_cols=" ++ show expandedCols

  let pRes = pairWiseDist (zip expandedRows expandedCols)
  putStrLn $ "Answer 1> " ++ show pRes

  let factExpRows = expandFactor rows missingRows 1000000
  print $ "fact_rows=" ++ show factExpRows
  let factExpCols = expandFactor cols missingCols 1000000
  print $ "fact_cols=" ++ show factExpCols
  let cRes = pairWiseDist (zip factExpRows factExpCols)
  putStrLn $ "Answer 2> " ++ show cRes
  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Pos = (Int, Int)

parseLine :: [Pos] -> Int -> [(Char, Int)] -> [Pos]
parseLine soFar row (('#', col):zs) = parseLine ((row, col):soFar) row zs
parseLine soFar row (('.', col):zs) = parseLine soFar row zs
parseLine soFar _ [] = soFar

missing :: [Int] -> [Int]
missing values = foldl opMissing [0..(maximum values)] values
  where opMissing l e = filter (/= e) l

expand :: [Int] -> [Int] -> [Int]
expand values missingValues = map (\v -> v + length (filter (< v) missingValues))  values

hamming :: Pos -> Pos -> Int
hamming (x1, y1) (x2, y2) = abs (x2 - x1) + abs(y2 - y1)

distTo :: Pos -> [Pos] -> Int
distTo pt listPt = sum ( map (hamming pt) listPt)

pairWiseDist :: [Pos] -> Int
pairWiseDist [] = 0
pairWiseDist (pt:pts) = distTo pt pts + pairWiseDist pts
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

expandFactor :: [Int] -> [Int] -> Int -> [Int]
expandFactor values missingValues factor = map (\v -> v + (factor - 1) * length (filter (< v) missingValues))  values
