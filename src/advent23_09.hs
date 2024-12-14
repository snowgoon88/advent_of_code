module Main where

-- import qualified MyParser as MP
-- import Data.List ( group, sort, sortOn, sortBy )
import Data.String.Utils ( replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )

l1 = "10 13 16 21 30 45"
i1 = map read (split " " l1) :: [Int]
l2 = "15 36 81 166 312 559 986 1730 2993 5021 8033 12071 16734 20750 21330 13237 -12509 -70382 -182663 -382436 -717404"
i2 = reverse $ parseLine l2

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 09 1 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input09.txt"
  -- content <- readFile "Input23/test09_1.txt"
  -- content <- readFile "Input23/test09_2.txt"

  let pRes = sum (map (augment . reduce . reverse . parseLine) (lines content))
  putStrLn $ "Answer 1> " ++ show pRes

  let cRes = sum (map (augmentInv . reduceInv . parseLine) (lines content))
  putStrLn $ "Answer 2> " ++ show cRes
  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
parseLine :: String -> [Int]
parseLine line = map read (split " " line)

diff :: [Int] -> [Int]
diff (n1:n2:ns) = (n1 - n2) : diff (n2:ns)
diff [n2] = []

reduce :: [Int] -> [[Int]]
reduce nums
  | all (== 0) differences = differences : [nums]
  | otherwise = reduce differences ++ [nums]
    where differences = diff nums

augment reduction = sum $ map head reduction

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************


diffInv :: [Int] -> [Int]
diffInv [n1,n2] = [n2 - n1]
diffInv (n1:n2:ns) = (n2 - n1) : diffInv (n2:ns)

reduceInv :: [Int] -> [[Int]]
reduceInv nums
  | all (== 0) differences = differences : [nums]
  | otherwise = reduceInv differences ++ [nums]
    where differences = diffInv nums

opAugment a b = b - a
augmentInv reduction = foldl opAugment 0 (map head reduction)
