{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
import MyGrid (Pos, Size, DirVec, GridMapCore, readGrid, addDir, isValidPos, chunks)
import Data.List ( sort, groupBy, group ) -- sortOn, groupBy, find, group, sort, sortBy )
-- import Data.Tuple ( swap )
-- import Data.String.Utils ( split ) --, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
import Data.Maybe ( fromJust, catMaybes, isNothing )
-- import Control.Monad ( fold )
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List ( delete, sortOn ) -- delete, sortOn
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import Control.Monad.State
-- *********************************************************************************** DEBUG
import Debug.Trace ( trace ) -- trace :: String > a -> a
traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x
-- used as (1+2) `debug` "adding"'
debug = flip trace

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2024 - Day 12 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input24/input12.txt"
  -- content <- readFile "Input24/test12_1.txt"
  -- content <- readFile "Input24/test12_2.txt"
  -- content <- readFile "Input24/test12_3.txt"
  -- content <- readFile "Input24/test12_4.txt"
  -- content <- readFile "Input24/test12_5.txt"

  let (size, grid) = readGridB (lines content)
  -- print $ "grid=" ++ show grid

  -- let regA = growRegion [] (size, grid) 'A' [(0, 0)]
  -- print $ "regA=" ++ show regA

  let allReg = findRegions 0 [] (size, grid)
  -- print $ "allReg=" ++ show (fst allReg)
  -- print "============="
  -- print $ "visited=" ++ show (snd allReg)



  let missings = map (\r -> (fst r, missingSides 0 (sort (snd r)))) (fst allReg)
  -- print $ "missings=" ++ show missings

  let features = map computeRegion (fst allReg)
  -- print $ "features=" ++ show features

  let prices = map priceRegion features
  -- print $ "prices=" ++ show prices

  let pRes = sum prices
  putStrLn $ "Answer 1> " ++ show pRes

  -- let fences = map (\r -> (fst r, followWall 0 (snd r) (head $ sort $ (snd r), (0,1))
  --                                                      (head $ sort $ (snd r)) (0,1)))
  --                  (fst allReg)
  -- print $ "fences=" ++ show fences

  -- let fences = map (computeFence (snd allReg)) (fst allReg)
  -- print $ "fences" ++ show fences

  -- let innerFences = addInnerFence fences fences
  -- print $ "innerFences=" ++ show innerFences

  -- let priceFences = map priceFence fences
  -- -- print $ "sum Fences= " ++ show (sum priceFences)
  -- -- print $ "priceFences=" ++ show priceFences

  -- let innerPrice = map priceFence innerFences
  -- -- print $ "innerPrice=" ++ show (sum innerPrice)

  -- let cRes = sum innerPrice

  let corners = map (\r -> (fst r, sort $ allCorners (snd r))) (fst allReg)
  -- print $ "corners=" ++ show corners

  let newFences = map (\r -> (fst r, (length (snd r), findFences (snd r)))) (fst allReg)
  -- print $ "newFences=" ++ show newFences

  let newPrices = map priceRegion newFences
  -- print $ "newPrices=" ++ show newPrices

  let cRes = sum newPrices
  putStrLn $ "Answer 2> " ++ show cRes


  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
-- Si on a le nombre d'arètes communes et le nb de cases, on peut en déduire
-- l'aire et le périmètre
-- aire = nb case
-- perim = 4 * nb case - 2 * nb_arêtes
-- ex X : 21 et 4 * 21 - 2 * 24 = 84 - 48 = 36
-- OU compter les voisins différents ?

-- Map Pos -> lettre, Viisited


readGridB :: [String] -> GridMap
readGridB lines = ((length lines, length (head lines)),
                   Map.fromList [((row, col), (c, Nothing)) | (row, line) <- zip [0..] lines, (col, c) <- zip [0..] line])

type Region = (Int, Int) -- nbCase, nbFence
type GridMap = GridMapCore (Char, Maybe Int)
type SeenMap = GridMapCore Bool

allDir = [(-1, 0), (0, -1), (1, 0), (0, 1)]

findRegions :: Int -> [((Char,Int), [Pos])] -> GridMap -> ([((Char,Int), [Pos])], GridMap)
findRegions id allReg (size, grid) = if length keysToVisit > 0
  then findRegions (id+1) (reg:allReg) newGrid
  else (allReg, (size, grid))
  where
    keysToVisit = Map.keys $ Map.filter (isNothing . snd) grid
    posStart = head keysToVisit
    (reg, newGrid) = growRegion [] (size, grid) id (fst $ grid Map.! posStart) [posStart]


growRegion :: [Pos] -> GridMap -> Int -> Char -> [Pos] -> (((Char, Int), [Pos]), GridMap)
growRegion acc gridMap id letter [] = (((letter, id), acc), gridMap)
growRegion acc (size, grid) id letter (pos:ps) = case getValGrid pos (size, grid) of
  (letter, Nothing) -> growRegion (pos:acc) (size, updatedGrid) id letter (ps ++ newPos)
  (_, _)          -> growRegion acc (size, grid) id letter ps
  where
    newPos = catMaybes (map (getNeighbor (size, grid) letter pos) allDir)
    updatedGrid = Map.insert pos (letter, Just id) grid
-- growRegion acc gridMap l p = error ("acc=" ++ show acc ++ ", g=" ++ show gridMap ++ " l=" ++ show l ++ ", p=" ++ show p)

getNeighbor :: GridMap -> Char -> Pos -> DirVec -> Maybe Pos
getNeighbor (sizeG, grid) letter pos dir
  | isValidPos sizeG neigh = case (c, seen) of
      (_, Just n)  -> Nothing
      (l, Nothing) -> if l == letter then Just neigh
                                   else Nothing
  | otherwise = Nothing
  where
    neigh = addDir pos dir
    (c, seen) = grid Map.! neigh

getValGrid :: Pos -> GridMap -> (Char, Maybe Int)
getValGrid pos (size, zeMap)
  | isValidPos size pos =  zeMap Map.! pos
  | otherwise = ('-', Nothing)

-- if we sort regions positions, looking for missing aretes is easier
missingSides :: Int -> [Pos] -> Int
missingSides nb [p] = nb
missingSides nb (p1:p2:ps)
  | addDir p1 (0,1) == p2 && elem (addDir p1 (1,0)) (p2:ps) = missingSides (nb+2) (p2:ps) -- `debug` ("p1="++ show p1 ++ " ->2")
  | addDir p1 (0,1) == p2 = missingSides (nb+1) (p2:ps) -- `debug` ("p1="++ show p1 ++ " ->1E")
  | addDir p1 (1,0) == p2 = missingSides (nb+1) (p2:ps) -- `debug` ("p1="++ show p1 ++ " ->1Sp")
  | elem (addDir p1 (1,0)) ps = missingSides (nb+1) (p2:ps) -- `debug` ("p1="++ show p1 ++ " ->1S")
  | otherwise = missingSides nb (p2:ps) -- `debug` ("p1="++ show p1 ++ " ->0")

computeRegion :: ((Char, Int), [Pos]) -> ((Char, Int), (Int, Int))
computeRegion (letter, posReg) = (letter, (area, 4 * area - 2 * missing ))
  where
    area = length posReg
    missing = missingSides 0 (sort posReg)

priceRegion :: ((Char, Int), (Int, Int)) -> Int
priceRegion (l, (a, p)) = a*p
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- follow wall at left

-- also infer the neigborings regions !!!!
followWall :: Int -> [Pos] -> GridMap -> [(Char, Int)] -> (Pos, DirVec) -> Pos -> DirVec -> (Int, [(Char, Int)])
followWall acc allPos (size, grid) regList startPose pos dir
  | (pos,dir) == startPose && acc > 0 = (acc, Set.toList $ Set.fromList regList)
  | elem (addDir pos (rotateLeft dir)) allPos = followWall (acc+1) allPos (size, grid)
                                                           (getRegionLeft (size,grid) (addDir pos (rotateLeft dir)) (rotateLeft dir) : regList)
                                                           startPose (addDir pos (rotateLeft dir)) (rotateLeft dir)
  | elem (addDir pos dir) allPos = followWall acc allPos (size,grid)
                                              (getRegionLeft (size, grid) pos dir : regList)
                                              startPose (addDir pos dir) dir
  | otherwise = followWall (acc+1) allPos (size, grid)
                           (getRegionLeft (size, grid) pos (rotateRight dir) : regList)
                            startPose pos (rotateRight dir)

getRegionLeft :: GridMap -> Pos -> DirVec -> (Char, Int)
getRegionLeft (size, grid) pos dir = case Map.lookup posLeft grid of
  Nothing -> ('*', -1)
  Just (l, Just n) -> (l,n)
  Just _ -> error ("pos=" ++ show pos ++ " should have been visited")
  where
    posLeft = addDir pos (rotateLeft dir)

computeFence :: GridMap -> ((Char, Int), [Pos]) -> ((Char, Int), (Int, (Int, [(Char, Int)])))
computeFence gridMap (letter, posReg) = (letter, (area, fences))
  where
    area = length posReg
    fences = followWall 0 posReg gridMap [] (head $ sort $ posReg, (0,1)) (head $ sort $ posReg) (0,1)

addInnerFence :: [((Char, Int), (Int, (Int, [(Char, Int)])))] -> [((Char, Int), (Int, (Int, [(Char, Int)])))] -> [((Char, Int), (Int, (Int, [(Char, Int)])))]
addInnerFence allFences [] = allFences
addInnerFence allFences (((l,id), (a, (f, regList))):fs)
  | length regList == 1 && (head regList) /= ('*', -1) = addInnerFence (alterFences [] allFences f (head regList)) fs
  | otherwise  = addInnerFence allFences fs
-- addInnerFence all left = error ("addInnerFence add=" ++ show all ++ " left=" ++ show left)

alterFences :: [((Char, Int), (Int, (Int, [(Char, Int)])))] -> [((Char, Int), (Int, (Int, [(Char, Int)])))] -> Int -> (Char, Int) -> [((Char, Int), (Int, (Int, [(Char, Int)])))]
alterFences acc [] _ (lReg, idReg) = error ("could not find " ++ show (lReg, idReg))
alterFences acc (((l,id), (a, (f, regList))):fs) fenceToAdd (lReg, idReg)
  | l == lReg && id == idReg =  acc ++ [((l,id), (a, (f+fenceToAdd, regList)))] ++ fs
  | otherwise = alterFences (((l,id), (a, (f, regList))):acc) fs fenceToAdd (lReg, idReg)


-- addToFence :: ((Char, Int), (Int, (Int, [(Char, Int)]))) -> Int -> ((Char, Int), (Int, (Int, [(Char, Int)])))
-- addToFence ((l,id), (a, (f, regList))) innerFence = ((l, id), (a, (f+innerFence, regList)))

priceFence :: ((Char, Int), (Int, (Int, [(Char, Int)]))) -> Int
priceFence ((l,id), (a, (f, _))) = a * f

rotateLeft :: DirVec -> DirVec
rotateLeft (-1,0) = (0,-1)
rotateLeft (0,-1) = (1,0)
rotateLeft (1,0) = (0,1)
rotateLeft (0,1) = (-1,0)

rotateRight :: DirVec -> DirVec
rotateRight (-1,0) = (0,1)
rotateRight (0,1) = (1,0)
rotateRight (1,0) = (0,-1)
rotateRight (0,-1) = (-1,0)

-- ********************************** Lets Play with corners ***********************
-- RU / RD / LU / LD
-- coord
data CType = RU | RD | LU | LD deriving (Ord, Eq, Show)
data Dir = R | D | L | U deriving (Ord, Eq, Show)
type Corner = (Pos, CType)

northPt allPos pos = elem (addDir pos (-1, 0)) allPos
westPt  allPos pos = elem (addDir pos (0, -1)) allPos
southPt allPos pos = elem (addDir pos (1, 0)) allPos
eastPt  allPos pos = elem (addDir pos (0, 1)) allPos
northWest allPos pos = elem (addDir pos (-1, -1)) allPos
southWest allPos pos = elem (addDir pos ( 1, -1)) allPos
southEast allPos pos = elem (addDir pos ( 1,  1)) allPos
northEast allPos pos = elem (addDir pos (-1,  1)) allPos

detectCorner :: [Pos] -> Pos -> [Corner]
detectCorner allPos (r,c) = outer ++ case (northPt allPos (r,c), westPt allPos (r,c), southPt allPos (r,c), eastPt allPos (r,c)) of
  (False, False, False, False ) -> [((r,c), RD), ((r,c+1), LD), ((r+1, c), RU), ((r+1, c+1), LU)]
  (False, False, False, True)   -> [((r,c), RD), ((r+1,c), RU)]
  (False, False, True, False)   -> [((r,c), RD), ((r,c+1), LD)]
  (False, True, False, False)   -> [((r,c+1), LD), ((r+1, c+1), LU)]
  (True, False, False, False)   -> [((r+1,c), RU), ((r+1,c+1), LU)]
  (False, False, True, True)    -> [((r,c), RD)]
  (True, False, False, True)    -> [((r+1, c), RU)]
  (False, True, True, False)    -> [((r,c+1), LD)]
  (True, True, False, False)    -> [((r+1,c+1), LU)]
  (_, _, _, _)                  -> []
  where
    outer = detectNW allPos (r,c) ++ detectSW allPos (r,c) ++ detectSE allPos (r,c) ++ detectNE allPos (r,c)

detectNW allPos (r,c)
  | northPt allPos (r,c) && westPt allPos (r,c) && not (northWest allPos (r,c)) = [((r,c), LU)]
  | otherwise = []
detectSW allPos (r,c)
  | southPt allPos (r,c) && westPt allPos (r,c) && not (southWest allPos (r,c)) = [((r+1,c), LD)]
  | otherwise = []
detectSE allPos (r,c)
  | southPt allPos (r,c) && eastPt allPos (r,c) && not (southEast allPos (r,c)) = [((r+1,c+1), RD)]
  | otherwise = []
detectNE allPos (r,c)
  | northPt allPos (r,c) && eastPt allPos (r,c) && not (northEast allPos (r,c)) = [((r,c+1), RU)]
  | otherwise = []


allCorners :: [Pos] -> [Corner]
allCorners allPos = foldl (\lc p -> lc ++ detectCorner allPos p) [] allPos


findFences :: [Pos] -> Int
findFences posReg = sum $ map length (allChains (sort (allCorners posReg)) [])


allChains :: [Corner] -> [[Corner]] -> [[Corner]]
allChains [] chains = chains
allChains (c:cs) chains = allChains (sort leftCorners) (newChain:chains)
  where (newChain, leftCorners) = findChain [] c R (c:cs)

findChain :: [Corner] -> Corner -> Dir -> [Corner] -> ([Corner], [Corner])
findChain chain ((r,c), RD) R allCorners = case nextL of
  Just ((rc, cc), LU) -> findChain (((rc,cc),LU):chain) ((rc, cc), LU) U (delete ((rc, cc), LU) allCorners)
  Just ((rc, cc), LD) -> findChain (((rc,cc),LD):chain) ((rc, cc), LD) D (delete ((rc, cc), LD) allCorners)
  Nothing -> (chain, allCorners)
  where
    nextL = findLeftC ((r,c), RD) allCorners
findChain chain ((r,c), RD) D allCorners = case nextU of
  Just ((rc, cc), LU) -> findChain (((rc,cc),LU):chain) ((rc, cc), LU) L (delete ((rc, cc), LU) allCorners)
  Just ((rc, cc), RU) -> findChain (((rc,cc),RU):chain) ((rc, cc), RU) R (delete ((rc, cc), RU) allCorners)
  Nothing -> (chain, allCorners)
  where
    nextU = findUpC ((r,c), RD) allCorners

findChain chain ((r,c), LD) D allCorners = case nextU of
  Just ((rc, cc), RU) -> findChain (((rc, cc), RU):chain) ((rc, cc), RU) R (delete ((rc, cc), RU) allCorners)
  Just ((rc, cc), LU) -> findChain (((rc, cc), LU):chain) ((rc, cc), LU) L (delete ((rc, cc), LU) allCorners)
  Nothing -> (chain, allCorners)
  where
    nextU = findUpC ((r, c), LD) allCorners
findChain chain ((r,c), LD) L allCorners = case nextR of
  Just ((rc, cc), RU) -> findChain (((rc, cc), RU):chain) ((rc, cc), RU) U (delete ((rc, cc), RU) allCorners)
  Just ((rc, cc), RD) -> findChain (((rc, cc), RD):chain) ((rc, cc), RD) D (delete ((rc, cc), RD) allCorners)
  Nothing -> (chain, allCorners)
  where
    nextR = findRightC ((r, c), LD) allCorners

findChain chain ((r,c), LU) U allCorners = case nextD of
  Just ((rc, cc), LD) -> findChain (((rc, cc), LD):chain) ((rc, cc), LD) L (delete ((rc, cc), LD) allCorners)
  Just ((rc, cc), RD) -> findChain (((rc, cc), RD):chain) ((rc, cc), RD) R (delete ((rc, cc), RD) allCorners)
  Nothing -> (chain, allCorners)
  where
    nextD = findDownC ((r, c), LU) allCorners
findChain chain ((r,c), LU) L allCorners = case nextR of
  Just ((rc, cc), RU) -> findChain (((rc, cc), RU):chain) ((rc, cc), RU) U (delete ((rc, cc), RU) allCorners)
  Just ((rc, cc), RD) -> findChain (((rc, cc), RD):chain) ((rc, cc), RD) D (delete ((rc, cc), RD) allCorners)
  Nothing -> (chain, allCorners)
  where
    nextR = findRightC ((r, c), LU) allCorners

findChain chain ((r,c), RU) R allCorners = case nextL of
  Just ((rc, cc), LU) -> findChain (((rc, cc), LU):chain) ((rc, cc), LU) U (delete ((rc, cc), LU) allCorners)
  Just ((rc, cc), LD) -> findChain (((rc, cc), LD):chain) ((rc, cc), LD) D (delete ((rc, cc), LD) allCorners)
  Nothing -> (chain, allCorners)
  where
    nextL = findLeftC ((r, c), RU) allCorners
findChain chain ((r,c), RU) U allCorners = case nextD of
  Just ((rc, cc), RD) -> findChain (((rc, cc), RD):chain) ((rc, cc), RD) R (delete ((rc, cc), RD) allCorners)
  Just ((rc, cc), LD) -> findChain (((rc, cc), LD):chain) ((rc, cc), LD) L (delete ((rc, cc), LD) allCorners)
  Nothing -> (chain, allCorners)
  where
    nextD = findDownC ((r, c), RU) allCorners

findLeftC :: Corner -> [Corner] -> Maybe Corner
findLeftC ((r,c),_) allCorners
  | length candidates > 0 = Just $ head $ sortOn (\((rc, cc),_) -> cc) candidates
  | otherwise = Nothing
  where
    candidates = filter (\((rc,cc), t) -> (rc == r) && (cc > c) && (t == LU || t == LD)) allCorners
findRightC :: Corner -> [Corner] -> Maybe Corner
findRightC ((r,c), _) allCorners
  | length candidates > 0 = Just $ head $ sortOn (\((rc, cc),_) -> (c-cc)) candidates
  | otherwise = Nothing
  where
    candidates = filter (\((rc,cc), t) -> (rc == r) && (cc < c) && (t == RU || t == RD)) allCorners
findUpC :: Corner -> [Corner] -> Maybe Corner
findUpC ((r,c), _) allCorners
  | length candidates > 0 = Just $ head $ sortOn (\((rc, cc),_) -> rc) candidates
  | otherwise = Nothing
  where
    candidates = filter (\((rc,cc), t) -> (rc > r) && (cc == c) && (t == RU || t == LU)) allCorners
findDownC :: Corner -> [Corner] -> Maybe Corner
findDownC ((r,c), _) allCorners
  | length candidates > 0 = Just $ head $ sortOn (\((rc, cc),_) -> (r-rc)) candidates
  | otherwise = Nothing
  where
    candidates = filter (\((rc,cc), t) -> (rc < r) && (cc == c) && (t == RD || t == LD)) allCorners
