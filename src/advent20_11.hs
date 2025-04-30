module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
import MyGrid (Pos, Size, DirVec, GridMapCore,
               readGrid, addDir, isValidPos, chunks, showGrid, getValMap, allDir)
-- import qualified MyCache as MC
-- ****** MyUtils: countTrue, groupLines

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe
--                    mapMaybe
import Data.Maybe ( mapMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl'
-- ****** Data.List.Extra: splitOn
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import qualified Control.Monad.Trans.State as St
-- import qualified Data.Bits as Bits
-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug = flip trace


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2020 - Day 11 Part - & -                                          **"
  putStrLn "********************************************************************************"
  -- content <- readFile "Input20/input11.txt"
  content <- readFile "Input20/test11_1.txt"

  let gMap = readGrid (lines content) :: GridMap
  -- putStrLn $ mapToStr gMap
  putStrLn $ showGrid gMap

  let seatPos = seats gMap
  -- putStrLn $ "seats=" ++ show seatPos

  -- let s1 = step gMap seatPos
  -- putStrLn "__STEP ********************"
  -- putStrLn $ mapToStr s1

  -- let s2 = step s1 seatPos
  -- putStrLn "__STEP ********************"
  -- putStrLn $ mapToStr s2

  let fixedPt1 = loop gMap nbOccupied 4 seatPos
  -- putStrLn "__FIXED ********************"
  -- putStrLn $ mapToStr fixedPt1

  let pRes = length (seatPos) - length (seats fixedPt1)
  putStrLn $ "Answer 1> " ++ show pRes

  let fixedPt2 = loop gMap countOccupied 5 seatPos
  -- putStrLn "__FIXED ********************"
  -- putStrLn $ mapToStr fixedPt2

  let cRes = length (seatPos) - length (seats fixedPt2)
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "__Lets us be smart ************************************************"
  let sMap = toSeatMap gMap
  -- putStrLn $ "sMap=" ++ show sMap
  let basicNMap = basicNeigborMap sMap
  let (_, smart1) = smartLoop 4 basicNMap sMap
  let nbSeat1 = length (filter (id) (Map.elems smart1))
  putStrLn $ "nbSeat1=" ++ show nbSeat1

  let sightNMap = sightNeigborsMap sMap
  let (_, smart2) = smartLoop 5 sightNMap sMap
  let nbSeat2 = length (filter (id) (Map.elems smart2))
  putStrLn $ "nbSeat2=" ++ show nbSeat2

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type GridMap = GridMapCore Char

mapToStr :: GridMap -> String
mapToStr (size, grid) = chunks (snd size) $ map snd (Map.toList grid)

seats :: GridMap -> [Pos]
seats gMap = Map.keys $ Map.filter (=='L') (snd gMap)

-- all 8 valid 8 neighbors
neighbors :: Size -> Pos -> [Pos]
neighbors size (row, col) = filter (isValidPos size) n_pos
  where
    n_pos = [(r, c) | r <- [(row-1) .. (row+1)], c <- [(col-1) .. (col+1)],
                      r /= row || c /= col ]

nbOccupied :: GridMap -> Pos -> Int
nbOccupied (size, gmap) pos = (length . filter (=='#')) neighVal
  where
    neighVal = map (\p -> getValMap (size, gmap) p '.') $ neighbors size pos

transition :: Int -> (GridMap, Int) -> (Pos, Int) -> (GridMap, Int)
transition thres ((size, gmap), nbChanged) (p, nbOcc) = case Map.lookup p gmap of
  Just 'L' -> if nbOcc == 0 then ((size, Map.insert p '#' gmap), nbChanged+1)
                       else ((size, gmap), nbChanged)
  Just '#' -> if nbOcc >= thres then ((size, Map.insert p 'L' gmap), nbChanged+1)
                       else ((size, gmap), nbChanged)
  _        -> ((size, gmap), nbChanged)

step :: GridMap -> (GridMap -> Pos -> Int) -> Int -> [Pos] -> (GridMap, Int)
step (size, gmap) fOcc thres posL = foldl (transition thres) ((size, gmap), 0) posOccL
  where
    posOccL = zip posL (map (fOcc (size, gmap)) posL)

-- 0.387s running not counteur => 0.376 with nbChanged counter => Not worthy
loop :: GridMap -> (GridMap -> Pos -> Int) -> Int -> [Pos] -> GridMap
loop gMap fOcc thres posL
  -- | mapToStr gMap == mapToStr newGMap = newGMap
  | nbChanged == 0 = newGMap
  | otherwise = loop newGMap fOcc thres posL
  where
    (newGMap, nbChanged) = step gMap fOcc thres posL

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

seeOccupied :: GridMap -> Pos -> DirVec -> Bool
seeOccupied gMap pos dir
  | getValMap gMap newpos 'L' == '.' = seeOccupied gMap newpos dir
  -- if not valid, gives 'L'
  | getValMap gMap newpos 'L' == 'L' = False
  | otherwise = True
  where
    newpos = addDir pos dir

countOccupied :: GridMap -> Pos -> Int
countOccupied gMap pos = (length . filter (id)) $ map (seeOccupied gMap pos) allDir

-- *****************************************************************************
-- *********************************************************************** Smart
-- *****************************************************************************

-- Grid: Map Pos (Bool) => only seats (Occ/Free), no vacant places.
type SeatMap = GridMapCore Bool

toSeatMap :: GridMap -> SeatMap
toSeatMap (size,  gmap) = (size, fmap (=='#') $ Map.filter (/= '.') gmap)

-- create a Map Pos (Set Pos) of neighbors for both settings
-- this is only computed once.
type NeighborsMap = GridMapCore [Pos]

basicNeigborMap :: SeatMap -> NeighborsMap
basicNeigborMap (size, smap) = (size, nmap)
  where
    nmap = Map.mapWithKey (\k _ -> filter (\p -> Map.member p smap) (neighbors size k)) smap

sightNeigborsDir :: SeatMap -> Pos -> DirVec -> Maybe Pos
sightNeigborsDir (size, smap) pos dir
  | isValidPos size newpos = case Map.lookup newpos smap of
      Just _ -> Just newpos
      Nothing -> sightNeigborsDir (size, smap) newpos dir
  | otherwise = Nothing
  where
    newpos = addDir pos dir

sightNeigborsMap :: SeatMap -> NeighborsMap
sightNeigborsMap (size, smap) = (size, nmap)
  where
    nmap = Map.mapWithKey (\k _ -> mapMaybe (sightNeigborsDir (size, smap) k) allDir) smap

smartStep
  :: Ord k => Int
     -> (a1, Map.Map k [k])
     -> (a2, Map.Map k Bool)
     -> (a2, Map.Map k Bool)
smartStep thres (_, nmap) (size, smap) = (size, Map.mapWithKey opMap smap)
  where
    -- (k -> a -> b)
    opMap pos occ
      | occ == True  = if nbOcc >= thres then False else True
      | otherwise    = if nbOcc == 0 then True else False
      where
        -- nbOcc = count_nb_True (map isOccupied listNeigbors(pos))
        nbOcc = (length . filter (id)) $ map (smap Map.!) (nmap Map.! pos)

smartLoop :: Int -> NeighborsMap -> SeatMap -> SeatMap
smartLoop thres nMap (size, smap)
  | Map.elems smap == Map.elems newmap = (size, newmap)
  | otherwise = smartLoop thres nMap (size, newmap)
  where
    (_, newmap) = smartStep thres nMap (size, smap)
