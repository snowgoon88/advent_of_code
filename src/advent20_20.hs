{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Use mapMaybe" #-}
module Main where

-- seems correct answer is 1688 :o)

import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore,
--                readGrid, addDir, isValidPos, chunks, showGrid, getValMap, allDir)
import qualified MyGrid as MG
-- import qualified MyCache as MC
-- ****** MyUtils: countTrue, groupLines
import qualified MyUtils as MU

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
import Data.Maybe ( catMaybes)
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Linear.V2 as L
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl, findIndex'
import Data.List (findIndex)
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

-- import qualified Text.Megaparsec            as P
-- import qualified Text.Megaparsec.Char       as P
-- import qualified Text.Megaparsec.Char.Lexer as PP
-- import Data.Void (Void)
-- exTileStr = "Tile 2311:\n..##.#..#.\n##..#.....\n#...##..#.\n####.#...#\n##.##.###.\n##...#.###\n.#.#.#..##\n..#....#..\n###...#.#.\n..###..###"
-- exTile = readTile (lines exTileStr)

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2020 - Day 20 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input20/input20.txt"
  -- content <- readFile "Input20/test20_1.txt"

  let groupTiles = MU.groupLines (lines content)
  let exTile = readTile (head groupTiles)
  -- putStrLn $ niceTile exTile
  -- putStrLn $ imgTile exTile
  let tiles = map readTile groupTiles
  let (dimSmallTiles, _) = strToImGrid (t_lines exTile)
  putStrLn $ "there are " ++ show (length tiles) ++ " tiles, size=" ++ show dimSmallTiles
  let nbTileOnGlobalSide = (floor . sqrt . fromIntegral) $ length tiles
  putStrLn $ "and the BigGlobalTile will be " ++ (show nbTileOnGlobalSide) ++ "x" ++ (show nbTileOnGlobalSide)
  let tileMap = Map.fromList $ map (\t -> (t_id t, t)) tiles
  let sideMap = foldl opMakeSideMap Map.empty tiles
  -- putStrLn $ "Sides:" ++ show sideMap

  let corners = extractCorners sideMap
  -- putStrLn $ "Corners: " ++ show corners
  let pRes = product (map head corners)
  putStrLn $ "Answer 1> " ++ show pRes


  -- gridTiles is the list of tranformed Tiles that will make the global Grid
  -- Tiles are listed row by row in the list.
  let gridTiles = foldl (opIndexMap tileMap sideMap) Map.empty [L.V2 r c | r <- [0..(nbTileOnGlobalSide - 1)],
                                                                c <- [0..(nbTileOnGlobalSide - 1)]]
  let strGridTiles = foldl opStrGT "" (Map.toList gridTiles)
        where
          opStrGT msg (L.V2 r c, tile) = msg ++ "[" ++ show r ++ ", " ++ show c ++ "]_" ++ show (t_id tile) ++ "; "
  -- putStrLn $ "gridTiles=" ++ show gridTiles
  putStrLn $ "gridTiles=" ++ strGridTiles

  -- bigGrid by concatenating all tiles, with a trick to regenerate valid global indexes
  -- TODO adapt to any size
  let bigGrid = Map.foldrWithKey opAddMap (nbTileOnGlobalSide*(dimSmallTiles-2), Map.empty) gridTiles
  -- make sure this bigGrid has no "hole" in its L.V2 index
  -- (as some indexes where removed when borders were removed)
  let bigGridNice = strToImGrid (lines (niceImGrid bigGrid))
  -- print "*** BigGridNice ************************"
  -- putStrLn $ niceImGrid bigGridNice

  let nbSea = map (`detectMonster` seaMonster) $ genGrids bigGridNice
  putStrLn $ "nbSea=" ++ show (map fst nbSea)

  let firstSolution = head $ filter (\(c, _) -> c > 0) nbSea
  -- print "==> masked Solution"
  -- putStrLn $ niceImGrid (snd firstSolution)
  let cRes = length (filter (== '#') (Map.elems (snd (snd firstSolution))))
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Side = (Int, Int)
sameSide :: Eq a => (a, b) -> (a, a) -> Bool
sameSide (s1,_) (s2,c2) = (s1 == s2) || (s1 == c2)
-- always lowest number first
orderedSide :: Ord b => (b, b) -> (b, b)
orderedSide (s,c) = if s < c then (s, c) else (c,s)
-- invert a side
invertSide :: Side -> Side
invertSide (s, c) = (c, s)
-- decode sides
asSide :: String -> Side
asSide str = (decode str, decode (reverse str))

decode :: String -> Int
decode  [] = 0
decode (x:xs) = if x == '#' then 1 + 2 * decode xs
                            else 2 * decode xs

-- Tile = id, rotation, flipped, [Side]
data Tile = Tile { t_id :: Int
                 , t_rot :: Int
                 , t_flipH :: Int
                 , t_flipV :: Int
                 , t_sides :: [Side]
                 , t_lines :: [String]
                 }
            deriving (Show)
type TileMap = Map.Map Int Tile
niceTile :: Tile -> String
niceTile tile = "Tile_" ++ show (t_id tile)
  ++ " (" ++ show (t_rot tile) ++ "/"
          ++ show (t_flipH tile) ++ "/"
          ++ show (t_flipV tile) ++ ")\n"
  ++ "  sides=" ++ show (t_sides tile)
imgTile :: Tile -> String
imgTile tile = unlines (t_lines tile)
imgTransTile :: Tile -> String
imgTransTile tile = niceImGrid (dim, transGrid)
  where
    (dim, grid) = strToImGrid (t_lines tile)
    transGrid = Map.mapKeys (coordTrans dim (t_rot tile) (t_flipH tile) (t_flipV tile)) grid

-- get opposite index
getFlippedIdx :: Int -> Int
getFlippedIdx i = 1 + mod ((mod (i-1) 4) + 2) 4

-- get the side given Tile+Transform+Index
-- index in [1, 2, 3, 4]
getTransformedIdx :: Int -> Int -> Int -> Int -> Int
getTransformedIdx rot flipH flipV index
  | flipH == 1 && even index = - getFlippedIdx rotIdx
  | flipV == 1 && odd index = - getFlippedIdx rotIdx
  | flipV == 1 && even index = - rotIdx
  | otherwise = rotIdx
  where
    rotIdx = 1 + mod ((index-1) - rot) 4
getTileSide :: Tile -> Int -> Side
getTileSide tile index = if transIdx > 0 then (s, c)
                                        else (c, s)
  where
    transIdx = getTransformedIdx (t_rot tile) (t_flipH tile) (t_flipV tile) index
    (s,c) = t_sides tile !! (abs transIdx - 1)



-- readTile
-- readTile :: [[Char]] -> (Int, (MG.Size, MG.PosMapCore Char), [[Char]], Tile)
-- readTile linesG = (MP.val idParse, (tileSize, tileGrid), [sideN, sideE, sideS, sideW], tile)
readTile :: [String] -> Tile
readTile linesG = tile
  where
    -- first line Tile id:
    idParse = MP.parseLabelID "Tile" (head linesG)
    --(tileSize, tileGrid) = MG.readGrid (tail linesG)
    -- gridLines is a list of lines
    gridLines = tail linesG
    sideN = head gridLines
    sideE = map last gridLines
    sideS = reverse (last gridLines)
    sideW = reverse (map head gridLines)
    tile = Tile (MP.val idParse) 0 0 0 (map asSide [sideN, sideE, sideS, sideW]) gridLines

-- a Map Side -> [id]
type SideMap = Map.Map Side [Int]
opMakeSideMap :: SideMap -> Tile -> SideMap
opMakeSideMap sMap tile = foldl opInsert sMap (t_sides tile)
  where
    opInsert sideMap side = Map.insertWith (++) (orderedSide side) [t_id tile] sideMap
-- what tiles have 2 sides with no corresponsdance in other tiles
-- return list of list of 1 ID
extractCorners :: SideMap -> [[Int]]
extractCorners sMap = Map.keys (Map.filter (== 2) countMap)
  where
    -- first: list of ids with no correspondance Sides
    borderMap = Map.filter (\ids -> length ids == 1) sMap
    -- count number of appearance of id in ids
    countMap = foldl (\m x -> Map.insertWith (+) x 1 m) Map.empty (Map.elems borderMap)
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- reconstruct the grid
-- map: coordinate -> Tile
type IndexMap = Map.Map Coord Tile

-- find the (inverted) indice of a Side in a Tile => to determine flip and rotation later
getIndex :: Tile -> Side -> Maybe Int
getIndex tile side = case index of
  Just idx -> if (t_sides tile !! idx) == side then Just (idx+1)
                                          else Just (-idx - 1)
  Nothing -> Nothing
  where
    index = findIndex (sameSide side) (t_sides tile)
-- for a corner (2 free sides), return indexes of free sides
freeSides :: SideMap -> Tile -> [Int]
freeSides sMap corner = catMaybes indexes
  where
    -- list of free orderedSides
    cornerOSides = Map.keys $ Map.filter (== [t_id corner]) sMap
    indexes = map (\s -> (+1) <$> findIndex (sameSide s) (t_sides corner)) cornerOSides
getBorderSideIdx :: SideMap -> Tile -> Int
getBorderSideIdx sMap borderTile = head $ freeSides sMap borderTile

-- rotation to the right, flipV, flipH
findTransform :: Int -> Int -> Maybe (Int, Int, Int)
findTransform idW idN
  -- only rotation
  | idW > 0 && idN > 0 && (idN == 1 + mod idW 4)              = Just (4 - idW, 0, 0)
  -- rotation and flipH
  | idW < 0 && idN < 0 && (mod (idN -1) (-4) == mod idW (-4)) = Just (4 + idW, 0, 1)
  -- -- rot, flipH, flipV
  -- | idW < 0 && idN < 0 && (mod idN (-4) == mod (idW-1) (-4)) = Just (mod (idN+3) 4, 1, 1)
  -- -- rot, flipH
  -- | idW < 0 && idN > 0 && idW == -(1 + mod idN 4)            = Just (mod (idW+2) 4, 1, 0)
  -- -- rot, flipV
  -- | idN < 0 && idW > 0 && mod idN (-4) == -(idW - 1)         = Just (mod (idN+3) 4, 0, 1)
  | otherwise = Nothing

-- find the neigboring tile
getNeighborTile :: TileMap -> SideMap -> Tile -> Int -> Tile
getNeighborTile tMap sMap tile idx = tMap Map.! idxTile
  where
    side = getTileSide tile idx
    bothTiles = sMap Map.! orderedSide side
    idxTile = head $ filter (/= (t_id tile)) bothTiles

transformTile :: Int -> Int -> Tile -> Maybe Tile
transformTile idxW idxN tile = case trans of
  Nothing -> Nothing
  Just (r, h, v) -> Just tile {t_rot=r, t_flipH=h, t_flipV=v}
  where
    trans = findTransform idxW idxN

-- Build up IndexMap
opIndexMap :: TileMap -> SideMap -> IndexMap -> Coord -> IndexMap
opIndexMap tMap sMap idxMap (L.V2 row col) = case newTile of
      Just tile -> Map.insert (L.V2 row col) tile idxMap
      Nothing   -> idxMap
  where
    tileLeft = Map.lookup (L.V2 row (col-1)) idxMap
    tileAbove = Map.lookup (L.V2 (row-1) col) idxMap
    newTile = opBuildNeighbor tMap sMap tileLeft tileAbove

opBuildNeighbor :: TileMap -> SideMap -> Maybe Tile -> Maybe Tile -> Maybe Tile
opBuildNeighbor tMap sMap (Just tileLeft) Nothing = Just newTileTrans --`debug` "opBN _ _ J N"
  where
    newTile = getNeighborTile tMap sMap tileLeft 2 --`debug` ("newTile=" ++ niceTile (getNeighborTile tMap sMap tileLeft 2))
    commonSideLeft = getTileSide tileLeft 2 --`debug` ("Left=" ++ show (getTileSide tileLeft 2))
    Just idxCommonSideRight = getIndex newTile (invertSide commonSideLeft) --`debug` ("idxRight=" ++ (show $ getIndex newTile (invertSide commonSideLeft)))
    idxSideAbove = freeSides sMap newTile --`debug` ("idxFreeAbove=" ++ (show $ freeSides sMap newTile))
    -- more than one freeSides => corner, but only ONE transform is possible
    -- if idxCommonSideRight < 0 then i also
    newTileTrans = head (catMaybes $ map (\i -> transformTile idxCommonSideRight (i * signum idxCommonSideRight) newTile) idxSideAbove)
opBuildNeighbor tMap sMap Nothing (Just tileAbove) = Just newTileTrans ----`debug` "opBN _ _ N J"
  where
    newTile = getNeighborTile tMap sMap tileAbove 3 --`debug` ("newTile=" ++ niceTile (getNeighborTile tMap sMap tileAbove 3))
    commonSideAbove = getTileSide tileAbove 3 --`debug` ("Above=" ++ show (getTileSide tileAbove 3))
    Just idxCommonSideDown = getIndex newTile (invertSide commonSideAbove) --`debug` ("idxDown=" ++ (show $ getIndex newTile (invertSide commonSideAbove)))
    idxSideLeft = freeSides sMap newTile --`debug` ("idxFreeLeft=" ++ (show $ freeSides sMap newTile))
    -- more than one freeSides => corner, but only ONE transform is possible
    newTileTrans = head (catMaybes $ map (\i -> transformTile (i * signum idxCommonSideDown) idxCommonSideDown newTile) idxSideLeft)
opBuildNeighbor tMap sMap (Just tileLeft) (Just tileAbove) = newTileTrans --`debug` "opBN _ _ J J"
  where
    newTile = getNeighborTile tMap sMap tileLeft 2 --`debug` ("newTile=" ++ niceTile (getNeighborTile tMap sMap tileLeft 2))
    commonSideLeft = getTileSide tileLeft 2 --`debug` ("Left=" ++ show (getTileSide tileLeft 2))
    Just idxCommonSideRight = getIndex newTile (invertSide commonSideLeft)
    commonSideAbove = getTileSide tileAbove 3 --`debug` ("Above" ++ show (getTileSide tileAbove 3))
    Just idxCommonSideDown = getIndex newTile (invertSide commonSideAbove)
    newTileTrans = transformTile idxCommonSideRight idxCommonSideDown newTile --`debug` ("trans " ++ show idxCommonSideRight ++ " " ++ show idxCommonSideDown)
opBuildNeighbor tMap sMap Nothing Nothing = newTile --`debug` "opBN _ _ N N"
  -- first corner: pick up the first one
  where
    corners = extractCorners sMap
    oneCorner = tMap Map.! (head (head corners))
    cornerSides = freeSides sMap oneCorner
    -- if [1,4] is a freeSide, then no rotation
    newTile = if cornerSides == [1,4] then Just oneCorner
                                      else transformTile (head cornerSides) (cornerSides !! 1) oneCorner

-- combine small maps into big one, removing borders
opAddMap :: Coord -> Tile -> ImGrid -> ImGrid
opAddMap  globalCoord localTile (dimGlobal, globalMap) =
  (dimGlobal, Map.foldrWithKey opAddChar globalMap borderLessTileGrid)
  where
    (dimLocal, wholeTileGrid) = strToImGrid (t_lines localTile)
    borderLessTileGrid = Map.filterWithKey (\(L.V2 row col) _ -> col > 0 && col < (dimLocal-1) && row > 0 && row < (dimLocal-1)) wholeTileGrid
    opAddChar localCoord c glMap = Map.insert (localCoordTrans + fmap (*dimLocal) globalCoord) c glMap
      where
        localCoordTrans = coordTrans dimLocal (t_rot localTile) (t_flipH localTile) (t_flipV localTile) localCoord

-- is SeaMonster (patterns begins at (rMin,cMin) and made of monsterPattern ) in ImGrid ?
-- returnd (nb of patterns matched, modified map where each pattern is a 'O')
detectMonster :: ImGrid -> (Coord, Coord, [Coord]) -> (Int, ImGrid)
detectMonster (dim, grid) (L.V2 rMin cMin, L.V2 rMax cMax, monsterPattern) = res
  where
    res = foldl opAlter (0, (dim, grid)) possibleOrigins
    possibleOrigins = Map.keys $ Map.filter (/= '.') (Map.filterWithKey (\(L.V2 row col) _ -> (row + rMin) > 0
                                                                          && (row + rMax) < dim
                                                                          && (col + cMin) > 0
                                                                          && (col + cMax) < dim) grid) :: [Coord]
    opAlter :: (Int, ImGrid) -> Coord -> (Int, ImGrid)
    opAlter (acc, (gDim, gMap)) origin = if detected then (acc+1, (gDim, alteredMap))
                                             else (acc, (gDim, gMap))
      where
        masked = maskedMapWith (gDim, gMap) monsterPattern origin
        detected = notElem '.' $ Map.elems masked
        alteredMap = foldl (\m k -> Map.adjust (const 'O') k m) gMap (Map.keys masked)

--maskedPatternAt :: ImGrid -> (Coord, Coord, [Coord]) -> Coord -> restricted Map
maskedMapWith :: ImGrid -> [Coord] -> Coord -> Map.Map Coord Char
maskedMapWith (_, grid) pat origin = masked
  where
    adaptedPatternSet ori = Set.fromList $ map (+ori) pat -- `debug` ("pat=" ++ show (Set.fromList $ map (+ori) pat))
    masked = Map.restrictKeys grid (adaptedPatternSet origin)

seaMonster :: (Coord, Coord, [Coord])
seaMonster = (L.V2 (-1) 0, L.V2 1 19,
              map (uncurry L.V2) [(0,0), (1,1), (1,4), (0,5), (0,6), (1,7), (1,10)
                                 ,(0,11), (0,12), (1,13), (1,16), (0,17), (0,18), (-1,18), (0,19)])

-- seek monster with rotation, flip
genGrids :: ImGrid -> [ImGrid]
genGrids (dim, gMap) = [(\ (rot, flipV)
                           -> (dim, Map.mapKeys (coordTrans dim rot 0 flipV) gMap))
                          (r, c) |
                          r <- [0 .. 3], c <- [0, 1]]

-- *****************************************************************************
-- ****************************************************** c/String operations
-- *****************************************************************************
-- As the "natural" ordering of L.V2 is (0, 0), (0, 1), (1, 0), (1, 1)
-- the indices will be (row, col)
type Coord = L.V2 Int
type CoordCharMap = Map.Map Coord Char
type ImGrid = (Int, CoordCharMap)

niceImGrid :: ImGrid -> String
niceImGrid (dim, iMap) = MG.chunks dim (Map.elems iMap)

strToImGrid :: [String] -> ImGrid
strToImGrid ls = (length ls, Map.fromList dataList)
  where
    dataList = [(L.V2 row col, c) | (row, l) <- zip [0..] ls
                         , (col, c) <- zip [0..] l]

-- transform Coord using rot, flipH, flipV
coordTrans :: Int -> Int -> Int -> Int -> Coord -> Coord
coordTrans dim rot flipH flipV coord = flipCoord dim flipH flipV rotC
  where
    rotC = MU.applyN (rotCoord dim) rot coord

rotCoord :: Int -> Coord -> Coord
rotCoord dim (L.V2 row col) = L.V2 col (dim-1 - row)

flipCoord :: Int -> Int -> Int -> Coord -> Coord
flipCoord dim flipH flipV (L.V2 row col) = L.V2 newRow newCol
  where
    newCol = (1-flipH)*col + flipH*(dim-1 - col)
    newRow = (1-flipV)*row + flipV*(dim-1 - row)
