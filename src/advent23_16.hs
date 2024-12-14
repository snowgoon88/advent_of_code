module Main where

import qualified MyParser as MP
import Data.List ( group ) --, sort, sortOn, sortBy )
-- import Data.Tuple ( swap )
-- import Data.String.Utils ( replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust )
-- import Control.Monad ( foldM )
import qualified Data.Map as Map
-- import Data.List ( sortOn )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 16 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input16.txt"
  -- content <- readFile "Input23/test16_1.txt"

  let grid = readGrid (lines content)
  --print $ "grid=" ++ show grid

  let nrjFull = flow grid Map.empty (0,0) East
  --print $ "nrjFull=" ++ show nrjFull

  let keyNRJ = map head ( group ( map fst (Map.keys nrjFull)))
  --print $ "keyNRJ=" ++ show keyNRJ

  let pRes = length keyNRJ
  putStrLn $ "Answer 1> " ++ show pRes

  -- **************************** Debug ****************************************
  let showNRJ grid dir pos = do
        let nrjPath = flow grid Map.empty pos dir
        -- print $ "nrjFull=" ++ show nrjPath

        let keyNRJ = map head ( group ( map fst (Map.keys nrjPath)))
        -- print $ "keyNRJ=" ++ show keyNRJ
        -- print $ "idxNRG=" ++ show (map (\(x, y) -> x * (snd(fst grid)) + y) keyNRJ)
        print $ "length = " ++ show (length keyNRJ)
        putStrLn (MP.chunks (snd(fst grid)) (niceNRJGrid grid nrjPath))

  let (nbRow, nbCol) = fst grid
  let flowLen = maximum $ map (opFlow grid South) [(0, y) | y <- [0..(nbCol - 1)]]
  print $ "flowLenSouth=" ++ show flowLen
  let flowLen = maximum $ map (opFlow grid North) [(nbRow - 1, y) | y <- [0..(nbCol - 1)]]
  print $ "flowLenNorth=" ++ show flowLen
  let flowLen = maximum $ map (opFlow grid East) [(x, 0) | x <- [0..(nbRow - 1)]]
  print $ "flowLenEast=" ++ show flowLen
  let flowLen = maximum $ map (opFlow grid South) [(x, (nbCol - 1)) | x <- [0..(nbRow - 1)]]
  print $ "flowLenWest=" ++ show flowLen

  -- mapM (showNRJ grid South) [(0, y) | y <- [0..(snd (fst grid) - 1)]]

  -- putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Pos = (Int, Int)
type GridMap = ((Int, Int), Map.Map Pos Char)

data Dir = North | West | South | East deriving (Eq, Ord, Show)
type NRJ = Map.Map (Pos, Dir) Int

readGrid :: [String] -> GridMap
readGrid lines = ((length lines, length (head lines)), Map.fromList [((row, col), c) | (row, line) <- zip [0..] lines, (col, c) <- zip [0..] line])

runBeam :: GridMap -> NRJ -> (Pos, Dir) -> NRJ
runBeam grid nrj (pos, dir) = nrj

flow :: GridMap -> NRJ -> Pos -> Dir -> NRJ
flow grid nrj (x, y) North
  | x < 0 = nrj          -- out of Bounds
  | otherwise = case Map.lookup ((x, y), North) nrj of
      Just _ -> nrj         -- already been there
      Nothing -> case (Map.lookup (x, y) (snd grid)) of
                   Just '.' -> flow grid updatedNRJ (x-1, y) North
                   Just '\\' -> flow grid updatedNRJ (x, y-1) West
                   Just '/' -> flow grid updatedNRJ (x, y+1) East
                   Just '|' -> flow grid updatedNRJ (x-1, y) North
                   Just '-' -> flow grid (flow grid updatedNRJ (x, y-1) West) (x, y+1) East
                   otherwise -> error ("unknown symbol in grid at (" ++ show x ++ ", " ++ show y ++ ")")
                   where updatedNRJ = Map.insert ((x, y), North) 1 nrj

flow grid nrj (x, y) West
  | y < 0 = nrj          -- out of Bounds
  | otherwise = case Map.lookup ((x, y), West) nrj of
      Just _ -> nrj         -- already been there
      Nothing -> case (Map.lookup (x, y) (snd grid)) of
                   Just '.' -> flow grid updatedNRJ (x, y-1) West
                   Just '\\' -> flow grid updatedNRJ (x-1, y) North
                   Just '/' -> flow grid updatedNRJ (x+1, y) South
                   Just '-' -> flow grid updatedNRJ (x, y-1) West
                   Just '|' -> flow grid (flow grid updatedNRJ (x-1, y) North) (x+1, y) South
                   otherwise -> error ("unknown symbol in grid at (" ++ show x ++ ", " ++ show y ++ ")")
                   where updatedNRJ = Map.insert ((x, y), West) 1 nrj

flow grid nrj (x, y) South
  | x >= (fst (fst grid)) = nrj          -- out of Bounds
  | otherwise = case Map.lookup ((x, y), South) nrj of
      Just _ -> nrj         -- already been there
      Nothing -> case (Map.lookup (x, y) (snd grid)) of
                   Just '.' -> flow grid updatedNRJ (x+1, y) South
                   Just '\\' -> flow grid updatedNRJ (x, y+1) East
                   Just '/' -> flow grid updatedNRJ (x, y-1) West
                   Just '|' -> flow grid updatedNRJ (x+1, y) South
                   Just '-' -> flow grid (flow grid updatedNRJ (x, y-1) West) (x, y+1) East
                   otherwise -> error ("unknown symbol in grid at (" ++ show x ++ ", " ++ show y ++ ")")
                   where updatedNRJ = Map.insert ((x, y), South) 1 nrj

flow grid nrj (x, y) East
  | y >= (snd (fst grid)) = nrj          -- out of Bounds
  | otherwise = case Map.lookup ((x, y), East) nrj of
      Just _ -> nrj         -- already been there
      Nothing -> case (Map.lookup (x, y) (snd grid)) of
                   Just '.' -> flow grid updatedNRJ (x, y+1) East
                   Just '\\' -> flow grid updatedNRJ (x+1, y) South
                   Just '/' -> flow grid updatedNRJ (x-1, y) North
                   Just '-' -> flow grid updatedNRJ (x, y+1) East
                   Just '|' -> flow grid (flow grid updatedNRJ (x-1, y) North) (x+1, y) South
                   otherwise -> error ("unknown symbol in grid at (" ++ show x ++ ", " ++ show y ++ ")")
                   where updatedNRJ = Map.insert ((x, y), East) 1 nrj
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
-- length of flow when entering grid from pos, dir
opFlow :: GridMap -> Dir -> Pos -> Int
opFlow grid dir pos = length (map head (group (map fst (Map.keys nrjFlow))))
  where nrjFlow = flow grid Map.empty pos dir

niceNRJGrid :: GridMap -> NRJ -> String
niceNRJGrid grid nrj = niceGrid 0 nrjKeys
  where
    nbCol = snd (fst grid)
    nbRow = fst (fst grid)
    nrjKeys = map (\(x,y) -> x*nbCol + y) $ map head ( group ( map fst (Map.keys nrj)))
    niceGrid :: Int -> [Int] -> String
    niceGrid idx (k:ks)
      | idx == k = '#' : niceGrid (idx+1) ks
      | otherwise = '.' : niceGrid (idx+1) (k:ks)
    niceGrid idx []
      | idx < nbRow * nbCol = '.' : niceGrid (idx+1) []
      | otherwise = ""
