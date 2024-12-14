module Main where

-- import qualified MyParser as MP
-- import Data.List ( group, sort, sortOn, sortBy )
import Data.String.Utils ( replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
import qualified Data.Massiv.Array as A
import Data.Maybe ( fromJust )
-- import Control.Monad ( foldM )
-- import qualified Data.Map as Map
-- import Data.List ( sortOn )


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 13 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input13.txt"
  -- content <- readFile "Input23/test13_1.txt"

  let separatedGrids = split [""] (lines content)
  -- print $ "sepGrids=" ++ show separatedGrids

  -- let g1 = fromJust (A.fromListsM A.Seq (head separatedGrids) :: Maybe MirrorMap)
  -- print $ "g1=" ++ show g1

  -- let g1T = A.toLists (A.transpose g1)
  -- print $ "g1T=" ++ show g1T

  -- print $ "symV=" ++ show (findSym g1T 0)
  -- print $ "symH=" ++ show (findSym (head separatedGrids) 0)

  -- let code = computeCode (head separatedGrids)
  -- print $ "code=" ++ show code

  let mapCode = map computeCode separatedGrids
  print $ "mapCode=" ++ show mapCode

  let pRes = sum mapCode
  putStrLn $ "Answer 1> " ++ show pRes

  -- let opStepUnfold curSum line = do
  --       let pat = parseLineUnfold line
  --       print "____"
  --       print pat

  --       let nbParsed = arrange (makeRowCursor pat)
  --       print $ "nbParsed=" ++ show nbParsed
  --       return (curSum + nbParsed)

  -- cRes <- foldM opStepUnfold 0 (lines content)
  let mapSmudge = map computeSmudge separatedGrids
  print $ "mapSmudge=" ++ show mapCode

  let cRes = sum mapSmudge
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

type MirrorMap = A.Array A.P A.Ix2 Char

-- build list[MirrorMap] where each MirroMap is a 2D Massiv.Array from list of lines
-- separated by a blank line

-- to check if symetry is between r and r+1,
-- nb row to compare = min (r+1, len - (r+1) )
-- compare [r-nb..r] with reverse [r+1..r+1+nb]
checkSym :: [String] -> Int -> Bool
checkSym grid idx = up == down
  where
    nbRow = min (idx+1) (length grid - (idx+1))
    up = take nbRow (drop (idx + 1 - nbRow) grid)
    down = reverse (take nbRow (drop (idx+1) grid))

findSym grid idx
  | idx == (length grid - 1) = Nothing
  | checkSym grid idx = Just (idx+1)
  | otherwise = findSym grid (idx+1)

computeCode gridLines =
  case findSym gridLines 0 of
    Nothing -> fromJust (findSym gridT 0)
    Just nb -> 100 * nb
  where gridT = A.toLists (A.transpose (fromJust (A.fromListsM A.Seq gridLines :: Maybe MirrorMap)))



-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

--nbSmudge up down = length ( filter (\(a,b) -> a /= b) (zip (concat up) (concat down)))
nbSmudge :: [String] -> [String] -> Int
nbSmudge up down = length ( filter (uncurry (/=)) (zip (concat up) (concat down)))

checkSymSmudge grid idx = nbSmudge up down == 1
  where
    nbRow = min (idx+1) (length grid - (idx+1))
    up = take nbRow (drop (idx + 1 - nbRow) grid)
    down = reverse (take nbRow (drop (idx+1) grid))

-- just for DEBUG
nbSymSmudge grid idx = nbSmudge up down
  where
    nbRow = min (idx+1) (length grid - (idx+1))
    up = take nbRow (drop (idx + 1 - nbRow) grid)
    down = reverse (take nbRow (drop (idx+1) grid))

findSmudge grid idx
  | idx == (length grid - 1) = Nothing
  | checkSymSmudge grid idx = Just (idx+1)
  | otherwise = findSmudge grid (idx+1)

computeSmudge gridLines =
  case findSmudge gridLines 0 of
    Nothing -> fromJust (findSmudge gridT 0)
    Just nb -> 100 * nb
  where gridT = A.toLists (A.transpose (fromJust (A.fromListsM A.Seq gridLines :: Maybe MirrorMap)))
