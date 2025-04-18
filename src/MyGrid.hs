module MyGrid where

-- import qualified MyGrid as MG

import qualified Data.Map as Map

type Pos = (Int, Int)
type Size = (Int, Int)
type DirVec = (Int, Int)

type PosMapCore a = Map.Map Pos a
type GridMapCore a = (Size, PosMapCore a)

-- *****************************************************************************
-- readSize : give (rmax, cmax) of 2D grid
-- *****************************************************************************
readSize :: [String] -> Size
readSize linesG = (length linesG, length (head linesG))

-- *****************************************************************************
-- readGrid : parse lines into ((nbRow, nbCol), Map (Int, Int) Char)
-- *****************************************************************************
readGrid :: [String] -> GridMapCore Char
readGrid linesG = ((length linesG, length (head linesG)), Map.fromList [((row, col), c) | (row, line) <- zip [0..] linesG, (col, c) <- zip [0..] line])


-- *****************************************************************************
-- readCoord : read the coord of every allowed character in [(Char, Pos)]
-- WARNING : specify NOT allowed
-- *****************************************************************************
readCoord :: [Char] -> [String] -> (Size, [(Char, Pos)])
readCoord notAllowed allLines = ((length allLines, length (head allLines)),
                             concat (map (\il -> parseLineCoord [] notAllowed (fst il) (zip (snd il) [0..]))
                                         (zip [0..] allLines) ))

parseLineCoord :: [(Char, Pos)] -> [Char] -> Int -> [(Char, Int)] -> [(Char, Pos)]
parseLineCoord soFar notAllowed row ((c, col):zs)
  | elem c notAllowed = parseLineCoord soFar notAllowed row zs
  | otherwise      = parseLineCoord ((c, (row, col)):soFar) notAllowed row zs
parseLineCoord soFar _ _ [] = soFar

-- *****************************************************************************
-- chunks : split a string in chunks of size n
-- *****************************************************************************
chunks :: Int -> String -> String
chunks _ [] = []
chunks n xs = ys ++ ('\n' : chunks n zs)
  where (ys, zs) = splitAt n xs

-- generate all DirVec pointing out of (0, 0)
allDir :: [DirVec]
allDir = [(r, c) | r <- [-1 .. 1], c <- [-1 .. 1], r /= 0 || c/= 0]
-- generate alle points inside [rLeft, rRight] x [cUp, cDown]
allPos :: Int -> Int -> Int -> Int -> [Pos]
allPos rLeft rRight cUp cDown = [(r, c) | r <- [rLeft .. rRight], c <- [cUp .. cDown]]

-- basic addition
addDir :: Pos -> DirVec -> Pos
addDir (r, c) (dr, dc) = (r+dr, c+dc)

-- inside SIze
isValidPos :: Size -> Pos -> Bool
isValidPos (rMax, cMax) (r, c) = r >= 0 && c >= 0 && r < rMax && c < cMax


-- *****************************************************************************
-- getVal : isValidPos more efficient than case ... of Notning ?
-- *****************************************************************************
getValMap :: GridMapCore a -> Pos -> a -> a
getValMap (size, zeMap) pos defValue
  | isValidPos size pos =  zeMap Map.! pos
  | otherwise = defValue

-- *****************************************************************************
-- modify: apply func to every Pos listed
-- *****************************************************************************
modifyMap :: Map.Map Pos a -> (Pos -> a) -> [Pos] -> Map.Map Pos a
modifyMap zeMap _ [] = zeMap
modifyMap zeMap func (pos:ps) = modifyMap (Map.insert pos (func pos) zeMap) func ps

-- modifyMapElem :: Map.Map Pos a -> (Pos -> a -> a) -> [Pos] -> Map.Map Pos a
-- modifyMapElem zeMap _ [] = zeMap
-- modifyMapElem zeMap func (pos:ps) = modifyMap (Map.adjust pos (func pos) zeMap) func ps
