module Main where

-- import qualified MyParser as MP
import Data.List ( group, sort, sortOn, sortBy )
import Data.Tuple ( swap )
-- import Data.String.Utils ( replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust )
import Control.Monad ( foldM )
import qualified Data.Map as Map
-- import Data.List ( sortOn )
import Data.Time.Clock.POSIX ( getPOSIXTime )


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 14 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input14.txt"
  -- content <- readFile "Input23/test14_1.txt"

  let gridSize = length (lines content)
  -- print $ "gridSize=" ++ show gridSize

  -- -- Naiv method ***************************************************************
  -- startNaiv <- getPOSIXTime
  -- let plat = initPlateform [] (head (lines content))
  -- -- print $ "plat_init=" ++ show plat

  -- -- let plat1 = map (updateColumn 2) (zip plat (head (drop 1 (lines content))))
  -- -- print $ "plat_1=" ++ show plat1

  -- let plat_fin = foldl opUpdate plat (zip [2..] (drop 1 (lines content)))
  -- -- print $ "plat_fin=" ++ show plat_fin

  -- let load = map (loadColumn gridSize) plat_fin
  -- -- print $ "load=" ++ show load
  -- let pRes = sum load
  -- endNaiv <- getPOSIXTime
  -- print $ "naivTime=" ++ show (endNaiv - startNaiv)
  -- -- ****************************************************************************
  -- putStrLn $ "Answer 1> " ++ show pRes

  let posChar = zip (generatePos gridSize (gridSize + 1)) content

  let posStones = map fst $ filter ((== 'O') . snd) posChar

  let posRock = map fst $ filter ((== '#') . snd) posChar
  let keyRockNorth = sort (wallKeys gridSize ++ map (codeNorth gridSize) posRock)
  let keyRockWest = sort (wallKeys gridSize ++ map (codeWest gridSize) posRock)
  let keyRockSouth = sort (wallKeys gridSize ++ map (codeSouth gridSize) posRock)
  let keyRockEast = sort (wallKeys gridSize ++ map (codeEast gridSize) posRock)

  -- convert to index method ****************************************************
  startKey <- getPOSIXTime
  -- let posChar = zip (generatePos gridSize (gridSize + 1)) content
  -- print $ "posStones=" ++ show posStones

  let keysStones = map (codeNorth gridSize) posStones
  let sortedKeyStones = sort keysStones
  -- print $ "keysStones=" ++ show keysStones
  -- print $ "sortedKeyStones=" ++ show sortedKeyStones

  -- print $ "posRock=" ++ show posRock

  let sortedKeyRock = keyRockNorth
  -- print $ "keysRock=" ++ show keysRock
  -- print $ "sortedKeyRock=" ++ show sortedKeyRock

  let stackedKeys = pileKeys sortedKeyRock sortedKeyStones
  -- print $ "stackedKeys=" ++ show stackedKeys
  let posNorth = map (decodeNorth gridSize) stackedKeys
  -- putStrLn (chunks 10 $ toGrid 10 posRock posNorth)
  -- print $ "posNorth=" ++ show posNorth

  let loadNorth = sum (map (loadPos gridSize) posNorth)
  endKey <- getPOSIXTime
  print $ "key Time=" ++ show (endKey - startKey)
  -- ****************************************************************************
  putStrLn $ "Answer 1> " ++ show loadNorth

  let testCode fnCode fnDecode sortedRockK = do
        let posStones = map fst $ filter ((== 'O') . snd) posChar
        let keysStones = map (fnCode gridSize) posStones
        let decode = map (fnDecode gridSize) keysStones
        print $ "posStones=" ++ show posStones
        print $ "keysStones=" ++ show keysStones
        print $ "decode=" ++ show decode

        let sortedStonesK = sort keysStones
        let stackedKeys = pileKeys sortedRockK sortedStonesK
        -- -- print $ "stackedKeys=" ++ show stackedKeys
        let posStacked = map (fnDecode gridSize) stackedKeys
        putStrLn (chunks gridSize $ toGrid gridSize posRock posStacked)

  -- print "__testing West"
  -- testCode codeWest decodeWest keyRockWest
  -- print "__testing South"
  -- testCode codeSouth decodeSouth keyRockSouth
  -- print "__testing East"
  -- testCode codeEast decodeEast keyRockEast

  let opCycleM stonesP idx = do
        print $ "__step " ++ show idx
        let step = cyclePlatform gridSize stonesP keyRockNorth keyRockWest keyRockSouth keyRockEast
        putStrLn (chunks gridSize $ toGrid gridSize posRock step)
        return step

  let opCycle stonesP _ = cyclePlatform gridSize stonesP keyRockNorth keyRockWest keyRockSouth keyRockEast



  -- let posStones = map fst $ filter ((== 'O') . snd) posChar
  -- let step01 = cyclePlatform gridSize posStones keyRockNorth keyRockWest keyRockSouth keyRockEast
  -- putStrLn (chunks gridSize $ toGrid gridSize posRock step01)

  -- print "__to North"
  -- let cycledNorthP = map (decodeNorth gridSize) (pileKeys keyRockNorth (sort (map (codeNorth gridSize) posStones)))
  -- putStrLn (chunks gridSize $ toGrid gridSize posRock cycledNorthP)

    -- cycledWestP = map (decodeWest size) (pileKeys westWallK (map (codeWest size) cycledNorthP))
    -- cycledSouthP = map (decodeSouth size) (pileKeys southWallK (map (codeSouth size) cycledWestP))
    -- cycledEastP = map (decodeEast size) (pileKeys eastWallK (map (codeEast size) cycledSouthP))

  -- foldM opCycleM posStones [1..10]
  startTime <- getPOSIXTime
  print "__FIND CYCLE start"
  let twist stonesP = cyclePlatform gridSize stonesP keyRockNorth keyRockWest keyRockSouth keyRockEast
  let res = findCycle posStones twist
  print "__FIND CYCLE end"
  endTime <- getPOSIXTime
  print $ "findCyle = " ++ show (fst res)

  let resMap = snd res
  print $ "mapSize=" ++ show (Map.size resMap)

  -- look for map at the right moment of the cycle
  let stonesFinal = foldl opCycle posStones [1 .. cycleIndex 1000000000 (fst res)]
  let cRes = sum $ map (loadPos gridSize) stonesFinal

  --let cycleIndex endIdx (first, repeat) = divMod (endIdx - first) repeat

  putStrLn $ "Answer 2> " ++ show cRes
  print $ "runing time = " ++ show (endTime - startTime)

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

data Column = Col { stones :: [Int]
                 , stop :: Int } -- rock or last stone
  deriving Show

initPlateform :: [Column] -> String -> [Column]
initPlateform plateform [] = reverse plateform
initPlateform plateform (c:cs)
  | c == 'O' = initPlateform (Col [1] 2:plateform) cs
  | c == '.' = initPlateform (Col [] 1:plateform) cs
  | c == '#' = initPlateform (Col [] 2:plateform) cs

updateColumn :: Int -> (Column, Char) -> Column
updateColumn rownb (col, 'O') = col {stones = lim:stones col, stop = lim + 1}
  where lim = stop col
updateColumn rownb (col, '.') = col
updateColumn rownb (col, '#') = col {stop=rownb+1}

opUpdate :: [Column] -> (Int, String) -> [Column]
opUpdate plateform (nb, l) = map (updateColumn nb) (zip plateform l)

loadStone :: Int -> Int -> Int
loadStone size pos = size - (pos -1)

loadColumn :: Int -> Column -> Int
loadColumn size col = sum (map (loadStone size) (stones col))

-- beware, here pos.x begins at 1
loadPos :: Int -> Pos -> Int
loadPos size (x, _) = size - (x - 1)
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

type Pos = (Int, Int)

-- generate the cartesian product, as (row, col)
generatePos :: Int -> Int -> [Pos]
generatePos nbRow nbCol = zip (concat $ map (take nbCol . repeat) [1..nbRow])
                              (take (nbCol * nbRow) (cycle [1..nbCol]))

wallKeys :: Int -> [Int]
wallKeys size = map (codeNorth size) (zip (repeat 0) [1..size])

-- North => in leftmost column first, up is better
codeNorth :: Int -> Pos -> Int
codeNorth size (x,y)  = x + y * (size+1)
decodeNorth :: Int -> Int -> Pos
decodeNorth size key = swap $ divMod key (size+1)

-- West => in bottom row first, left is better
codeWest :: Int -> Pos -> Int
codeWest size (x,y)  = (size + 1 - x) * (size+1) + y
decodeWest :: Int -> Int -> Pos
decodeWest size key = (size - a + 1, b)
  where (a, b) = divMod key (size+1)

-- South => in right column first, down is better
codeSouth :: Int -> Pos -> Int
codeSouth size (x,y)  = (size + 1 - x) + (size+1) * (size + 1 - y)
decodeSouth :: Int -> Int -> Pos
decodeSouth size key = (size - b + 1, size - a + 1)
  where (a, b) = divMod key (size+1)

-- East => in up row first, right is better
codeEast :: Int -> Pos -> Int
codeEast size (x,y)  = (size+1) * x + (size + 1 - y)
decodeEast :: Int -> Int -> Pos
decodeEast size key = (a, size - b + 1)
  where (a, b) = divMod key (size+1)

pileKeys :: [Int] -> [Int] -> [Int]
pileKeys (bot:top:rs) stones = piledStones ++ pileKeys (top:rs) leftStones
  where
    nbTaken = length (takeWhile (<top) stones)
    piledStones = [bot+1 .. bot+nbTaken]
    leftStones = drop nbTaken stones
pileKeys [bot] stones = piledStones
  where
    nbTaken = length stones
    piledStones = [bot+1 .. bot+nbTaken]

cyclePlatform size stonesP northWallK westWallK southWallK eastWallK = cycledEastP
  where
    cycledNorthP = map (decodeNorth size) (pileKeys northWallK (sort (map (codeNorth size) stonesP)))
    cycledWestP = map (decodeWest size) (pileKeys westWallK (sort (map (codeWest size) cycledNorthP)))
    cycledSouthP = map (decodeSouth size) (pileKeys southWallK (sort (map (codeSouth size) cycledWestP)))
    cycledEastP = map (decodeEast size) (pileKeys eastWallK (sort (map (codeEast size) cycledSouthP)))


--findCycle -> ((first, second), Map.ofStonesP)
type StoneTimeMap = Map.Map [Pos] Int

findCycle :: [Pos] -> ([Pos] -> [Pos]) -> ((Int, Int), StoneTimeMap)
findCycle startStonesPos funcCycle = findCycleInner 0 startStonesPos Map.empty
  where
    findCycleInner curTime stonesPos mapStoneTime = hasSeen (Map.lookup stonesPos mapStoneTime)
      where
        hasSeen (Just firstTime) = ((firstTime, curTime), mapStoneTime)
        hasSeen Nothing = findCycleInner (curTime+1) (funcCycle stonesPos)
                                         (Map.insert stonesPos curTime mapStoneTime)

cycleIndex :: Int -> (Int, Int) -> Int
cycleIndex endIdx (first, repeat) = first + rem (endIdx - first) (repeat - first)

-- *****************************************************************************
-- DEBUG display back **********************************************************
-- *****************************************************************************
-- afficher: putStrLn (chunks 10 $ toGrid 10 posRock posNorth)
codeDisplay :: Int -> Pos -> Int
codeDisplay size (x, y) = x * (size + 1) + y
decodeDisplay :: Int -> Int -> Pos
decodeDisplay size key = divMod key (size+1)

genGrid :: Int -> Int -> [Int] -> [Int] -> String
genGrid size idx (irock:rs) (istone:ss)
  | mod idx (size+1) == 0 = genGrid size (idx+1) (irock:rs) (istone:ss)
  | idx == irock = '#' : genGrid size (idx+1) rs (istone:ss)
  | idx == istone = 'O' : genGrid size (idx+1) (irock:rs) ss
  | otherwise = '.' : genGrid size (idx+1) (irock:rs) (istone:ss)
genGrid size idx (irock:rs) []
  | mod idx (size+1) == 0 = genGrid size (idx+1) (irock:rs) []
  | idx == irock = '#' : genGrid size (idx+1) rs []
  | otherwise = '.' : genGrid size (idx+1) (irock:rs) []
genGrid size idx [] (istone:ss)
  | mod idx (size+1) == 0 = genGrid size (idx+1) [] (istone:ss)
  | idx == istone = 'O' : genGrid size (idx+1) [] ss
  | otherwise = '.' : genGrid size (idx+1) [] (istone:ss)
genGrid size idx rocks stones = '.' : genGrid size (idx+1) rocks stones

toGrid :: Int -> [Pos] -> [Pos] -> String
toGrid size rocksP stonesP = take (size*size) (genGrid size (codeDisplay size (1,1)) rocksK stonesK)
  where
    rocksK = sort $ map (codeDisplay size) rocksP
    stonesK = sort $ map (codeDisplay size) stonesP

chunks :: Int -> String -> String
chunks _ [] = []
chunks n xs = ys ++ ('\n' : chunks n zs)
  where (ys, zs) = splitAt n xs
