module Main where

-- import qualified MyParser as MP
-- import Data.List ( group, sort, sortOn, sortBy )
-- import Data.String.Utils ( replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
import qualified Data.Massiv.Array as A
import Data.Maybe ( fromJust )

l = [[1, 2, 3], [11, 12, 13], [21, 22, 23]]
justm = A.fromListsM A.Seq l :: Maybe( A.Array A.P A.Ix2 Int)
mm = fromJust justm
bm = A.replicate A.Seq (A.size mm) 0 :: A.Array A.P A.Ix2 Int
--mut_bm = A.thaw bm

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 10 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input10.txt"
  -- content <- readFile "Input23/test10_1.txt"
  -- content <- readFile "Input23/test10_2.txt"
  -- content <- readFile "Input23/test10_3.txt"
  -- content <- readFile "Input23/test10_4.txt"
  -- content <- readFile "Input23/test10_5.txt"

  let justArray = A.fromListsM A.Seq (lines content) :: Maybe (A.Array A.P A.Ix2 Char)
  let arrayMap = fromJust justArray
  print "__Array of the map"
  print arrayMap

  let posS = head $ A.ifoldlS (opFilter 'S') [] arrayMap
  print $ "'S' is at " ++ show posS -- row, col

  let okMoves = map (\d -> isLegit d (A.index arrayMap (move posS d))) [No, So, Ea, We]
  print $ "okMove=" ++ show okMoves

  let validMove = findLegitMove arrayMap posS [No, So, Ea, We]
  print $ "validMove=" ++ show validMove
  let firstCell = nextPos posS (fromJust validMove) (arrayMap A.! (move posS (fromJust validMove)))
  print $ "firstCell=" ++ show firstCell

  let pathLen = moveAlong arrayMap (0, posS, fromJust validMove) []
  print $ "path " ++ show pathLen
  print $ "pathLen=" ++ show (div (length pathLen) 2)
  -- putStrLn $ "Answer 1> " ++ show pRes

  -- build new map of the same size
  let bareMap = A.replicate A.Seq (A.size arrayMap) '.' :: A.Array A.P A.Ix2 Char
  -- print "__bareMap"
  -- print bareMap

  mut_bm <- A.thaw bareMap
  -- A.write_ mut_bm (1 A.:. 1) 'X'
  -- newm <- A.freeze A.Seq mut_bm
  -- mapM_ (\pos_c -> A.write_ mut_bm (fst pos_c) (translate (snd pos_c))) pathLen
  mapM_ (\pos_c -> A.write_ mut_bm (fst pos_c) (snd pos_c)) pathLen
  A.write_ mut_bm posS (startSymbol okMoves)
  new_map <- A.freeze A.Seq mut_bm
  print "__bareMap"
  print new_map

  let mapAsList = A.toLists new_map
  -- print "__as list"
  -- print mapAsList

  -- mapM_ (print . (\row -> countInside 0 (Outside, row))) mapAsList
  let cRes = sum (map (\row -> countInside 0 (Outside, row)) mapAsList)

  putStrLn $ "Answer 2> " ++ show cRes
  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type MapArray = A.Array A.P A.Ix2 Char

data Dir = No | So | Ea | We
  deriving Show
move pos No = pos + A.Ix2 (-1) 0
move pos So = pos + A.Ix2 1 0
move pos Ea = pos + A.Ix2 0 1
move pos We = pos + A.Ix2 0 (-1)

-- ifoldlS :: (Index ix, Source r e) => (a -> ix -> e -> a) -> a -> Array r ix e -> a
-- where is 'S'
opFilter :: Char -> [A.Ix2] -> A.Ix2 -> Char -> [A.Ix2]
opFilter charToFind listIndex idx elemArray
  | elemArray == charToFind = idx:listIndex
  | otherwise = listIndex

-- scan around 'S' for legit Dir


-- opMove (length, lastDir)
-- moveAlong :: MapArray -> (Int, A.Ix2, Dir) -> Int
-- moveAlong mapArray (pathLong, pos, dir)
--   | nextCell == 'S' = pathLong
--   | otherwise = moveAlong mapArray (pathLong+1, newPos, nextDir)
--   where nextCell = mapArray A.! newPos
--         (nextDir, newPos) = nextPos pos dir nextCell

moveAlong :: MapArray -> (Int, A.Ix2, Dir) -> [(A.Ix2, Char)] -> [(A.Ix2, Char)]
moveAlong mapArray (pathLong, pos, dir) soFar
  | cellReached == 'S' = (move pos dir, cellReached):soFar
  | otherwise = moveAlong mapArray (pathLong+1, newPos, nextDir)
                               ((newPos, nextCell):soFar)
  where cellReached = mapArray A.! (move pos dir)
        (newPos, nextDir) = nextPos pos dir cellReached
        nextCell = mapArray A.! newPos

moveAlongDebug :: Int -> MapArray -> (Int, A.Ix2, Dir) -> [(A.Ix2, Char)] -> [(A.Ix2, Char)]
moveAlongDebug maxLong mapArray (pathLong, pos, dir) soFar
  | pathLong >= maxLong = soFar
  | cellReached == 'S' = (move pos dir, cellReached):soFar
  | otherwise = moveAlongDebug maxLong mapArray (pathLong+1, newPos, nextDir)
                               ((newPos, nextCell):soFar)
  where cellReached = mapArray A.! (move pos dir)
        (newPos, nextDir) = nextPos pos dir cellReached
        nextCell = mapArray A.! newPos

-- look for starting route around start
findLegitMove :: A.Array A.P A.Ix2 Char -> A.Ix2 -> [Dir] -> Maybe Dir
findLegitMove _ _ [] = Nothing
findLegitMove mapArray pos (d:ds)
  | legitMove = Just d
  | otherwise = findLegitMove mapArray pos ds
    where legitMove = isLegit d (A.index mapArray (move pos d))

-- nextPos arrive from `pos`, following direction Dir, on cell `c`
nextPos :: A.Ix2 -> Dir -> Char -> (A.Ix2, Dir)
nextPos pos No 'F' = (move pos No, Ea)
nextPos pos No '|' = (move pos No, No)
nextPos pos No '7' = (move pos No, We)
nextPos pos No c = error ("From " ++ show pos ++ ": " ++ show No ++ " at " ++ [c])

nextPos pos So 'L' = (move pos So, Ea)
nextPos pos So '|' = (move pos So, So)
nextPos pos So 'J' = (move pos So, We)
nextPos pos So c = error ("From " ++ show pos ++ ": " ++ show So ++ " at " ++ [c])

nextPos pos Ea '7' = (move pos Ea, So)
nextPos pos Ea '-' = (move pos Ea, Ea)
nextPos pos Ea 'J' = (move pos Ea, No)
nextPos pos Ea c = error ("From " ++ show pos ++ ": " ++ show Ea ++ " at " ++ [c])

nextPos pos We 'F' = (move pos We, So)
nextPos pos We '-' = (move pos We, We)
nextPos pos We 'L' = (move pos We, No)
nextPos pos We c = error ("From " ++ show pos ++ ": " ++ show We ++ " at " ++ [c])

-- legitMove ?
isLegit :: Dir -> Maybe Char -> Bool
isLegit _ Nothing = False
isLegit No (Just c) = elem c ['F', '|', '7']
isLegit So (Just c) = elem c ['L', '|', 'J']
isLegit Ea (Just c) = elem c ['7', '-', 'J']
isLegit We (Just c) = elem c ['F', '-', 'L']

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- when writing back to the Mutable map, transform some symbols into otherwise
-- '|' and '-' are kept
-- others are '+'
translate :: Char -> Char
translate '|' = '|'
translate '-' = '-'
translate _ = '+'

startSymbol [False, True, True, False] = 'F'
startSymbol [False, True, False, True] = '7'
startSymbol [True, False, True, False] = 'L'
startSymbpm [True, False, False, True] = 'J'

data PathState = Outside | OutsideUp | OutsideBot | Inside | InsideUp | InsideBot

countInside :: Int -> (PathState, String) -> Int
countInside count (_, []) = count

countInside count (Outside, '.':ls) = countInside count (Outside, ls)
countInside count (Outside, '|':ls) = countInside count (Inside, ls)

countInside count (Outside, 'F':ls) = countInside count (OutsideBot, ls)
countInside count (OutsideBot, '-':ls) = countInside count (OutsideBot, ls)
countInside count (OutsideBot, 'J':ls) = countInside count (Inside, ls)
countInside count (OutsideBot, '7':ls) = countInside count (Outside, ls)

countInside count (Outside, 'L':ls) = countInside count (OutsideUp, ls)
countInside count (OutsideUp, '-':ls) = countInside count (OutsideUp, ls)
countInside count (OutsideUp, 'J':ls) = countInside count (Outside, ls)
countInside count (OutsideUp, '7':ls) = countInside count (Inside, ls)


countInside count (Inside, '.':ls) = countInside (count+1) (Inside, ls)
countInside count (Inside, '|':ls) = countInside count (Outside, ls)

countInside count (Inside, 'F':ls) = countInside count (InsideBot, ls)
countInside count (InsideBot, '-':ls) = countInside count (InsideBot, ls)
countInside count (InsideBot, '7':ls) = countInside count (Inside, ls)
countInside count (InsideBot, 'J':ls) = countInside count (Outside, ls)

countInside count (Inside, 'L':ls) = countInside count (InsideUp, ls)
countInside count (InsideUp, '-':ls) = countInside count (InsideUp, ls)
countInside count (InsideUp, 'J':ls) = countInside count (Inside, ls)
countInside count (InsideUp, '7':ls) = countInside count (Outside, ls)
