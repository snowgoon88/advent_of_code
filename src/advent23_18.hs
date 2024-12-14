{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid )
import Data.List ( sortOn, groupBy ) --, find ) --group ) --, sort, , sortBy )
-- import Data.Tuple ( swap )
import Data.String.Utils ( split ) --replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust )
-- import Control.Monad ( fold )
-- import qualified Data.Map as Map
-- import Data.List ( sortOn )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
import Numeric ( readHex )


main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 18 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input18.txt"
  -- content <- readFile "Input23/test18_1.txt"

  let cmds = map readLine (lines content)
  print $ "len cmds=" ++ show (length cmds)
  print $ "pathLen=" ++ show (lenPath cmds)

  let cells = foldl opExpand [] (zip cmds (drop 1 cmds ++ [head cmds]))
  -- print $ "cells=" ++ show cells

  -- the sort from top to bottom, removing "start"
  let sortedCells = sortOn (fst . fst) cells
  -- print $ "sortedCells=" ++ show sortedCells
  let grouped = groupBy (\c1 c2 -> fst (fst c1) == fst (fst c2)) sortedCells
  -- print $ "grouped=" ++ show grouped
  mapM_ (print . (map niceCell)) grouped

  let segs = map (filterSegments [] . sortOn (snd . fst)) grouped
  let segs2 = map (filterSegments2 [] . sortOn (snd . fst)) grouped
  -- print $ "segs=" ++ show segs2
  mapM_ (print . (map niceSeg)) segs2
  -- let fileWithSeg = concat (map (map fileSeg) segs2)
  -- print $ "fileWithSeg=" ++ show fileWithSeg
  -- mapM_ putStr fileWithSeg

  let segLen = concat $ map (map segmentSize) segs
  print $ "segLen=" ++ show (sum segLen)
  let segLen2 = concat $ map (map segmentSize) segs2

  let pRes = (lenPath cmds) + (sum segLen)
  putStrLn $ "Answer 1> " ++ show pRes
  let pRes2 = (lenPath cmds) + (sum segLen2)
  putStrLn $ "Answer 1> " ++ show pRes2

  let newCmds = map correctCmd cmds
  -- print $ "newCmds=" ++ show newCmds

  let cells2 = foldl opExpand [] (zip newCmds (drop 1 newCmds ++ [head newCmds]))
  -- print $ "cells=" ++ show cells

  -- the sort from top to bottom, removing "start"
  let sortedCells2= sortOn (fst . fst) cells2
  -- print $ "sortedCells=" ++ show sortedCells
  let grouped2 = groupBy (\c1 c2 -> fst (fst c1) == fst (fst c2)) sortedCells2
  -- print $ "grouped=" ++ show grouped
  -- mapM_ (print . (map niceCell)) grouped

  -- let segs = map (filterSegments [] . sortOn (snd . fst)) grouped
  let segs3 = map (filterSegments2 [] . sortOn (snd . fst)) grouped2

  let segLen3 = concat $ map (map segmentSize) segs3

  -- let pRes = (lenPath cmds) + (sum segLen)
  -- putStrLn $ "Answer 1> " ++ show pRes
  print $ "pathLen=" ++ show (lenPath newCmds)

  let cRes = (lenPath newCmds) + (sum segLen3)
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Pos = (Int, Int)
type Cell = (Pos, Cmd)
type Cmd = (String, Int, String)

dir :: Cell -> String
dir (_, (c, _, _)) = c
nbCell :: Cmd -> Int
nbCell (_, nb, _) = nb

readLine :: String -> Cmd
readLine line = ( head tokens, nb, color)
  where
    tokens = split " " line
    nb = read (head (drop 1 tokens)) :: Int
    color = head (drop 2 tokens)

correctCmd :: Cmd -> Cmd
correctCmd (_, _, color) = (newDir, nbCode, color)
  where
    nbCode = fst ( head ( readHex (drop 2 (take 7 color)) ))
    dirCode = read (take 1 (drop 7 color)) :: Int
    newDir = ["R", "D", "L", "U"] !! dirCode

--cmdToList :: [Cmd] -> [Cell]
opExpand :: [Cell] -> (Cmd, Cmd) -> [Cell]
opExpand [] (cmd, cmdNext) = expand (0, 0) cmd cmdNext
opExpand cells (cmd, cmdNext) = expand pos cmd cmdNext ++ cells
  where (pos, color) = head cells

-- list segment to fill : pair of (x,y1), (x, y2) Pos with no R or L points
filterSegments :: [(Pos,Pos)] ->  [Cell] -> [(Pos, Pos)]
filterSegments segs [] = segs
filterSegments segs [c] = segs
filterSegments segs (c1:c2:cs)
  -- | dir c1 == "LU" = filterSegments segs cs
  -- | dir c1 == "LD" = filterSegments segs (c2:cs)
  -- | dir c2 == "RD" = filterSegments segs (c2:cs)
  -- | dir c2 == "RU" = filterSegments segs cs
  | dir c1 == "LU" = filterSegments segs (c2:cs)
  | dir c1 == "LD" = filterSegments segs cs
  | dir c2 == "RU" = filterSegments segs (c2:cs)
  | dir c2 == "RD" = filterSegments segs cs
  | otherwise = filterSegments ((fst c1, fst c2):segs) cs
filterSegments2 :: [(Pos,Pos)] ->  [Cell] -> [(Pos, Pos)]
filterSegments2 segs [] = segs
filterSegments2 segs [c] = segs
filterSegments2 segs (c1:c2:cs)
  | dir c1 == "LD" && dir c2 == "DL" = filterSegments2 segs (c2:cs) -- Arete
  | dir c1 == "LD" && dir c2 == "UL" = filterSegments2 segs (c2:cs) -- Arete
  | dir c1 == "LU" && dir c2 == "DL" = filterSegments2 segs (c2:cs) -- Arete
  | dir c1 == "LU" && dir c2 == "UL" = filterSegments2 segs (c2:cs) -- Arete
  | dir c1 == "DL" || dir c1 == "DR" = filterSegments2 segs (c2:cs) -- outside

  | dir c1 == "UR" && dir c2 == "RD" = filterSegments2 segs (c2:cs) -- Arete
  | dir c1 == "UR" && dir c2 == "RU" = filterSegments2 segs (c2:cs) -- Arete
  | dir c1 == "DR" && dir c2 == "RU" = filterSegments2 segs (c2:cs) -- Arete
  | dir c1 == "DR" && dir c2 == "RD" = filterSegments2 segs (c2:cs) -- Arete
  | dir c2 == "UL" || dir c2 == "UR" = filterSegments2 segs (c2:cs) -- outside

  | dir c1 == "RD" || dir c2 == "LU" = filterSegments2 segs (c2:cs) -- outside

  | otherwise = filterSegments2 ((fst c1, fst c2):segs) cs

niceCell :: Cell -> String
niceCell (pos, (d, _, _)) = show pos ++ ":" ++ show [d] ++ ", "
niceSeg (pos1, pos2) = show pos1 ++ ":" ++ show pos2
fileSeg ((r1, c1), (r2, c2)) = show r1 ++ " " ++ show c1 ++ " " ++ show r2 ++ " " ++ show c2 ++ "\n"

segmentSize :: (Pos, Pos) -> Int
segmentSize ((_, c1), (_, c2)) = c2 - c1 - 1

lenPath :: [Cmd] -> Int
lenPath cmds = sum (map nbCell cmds)

-- Cmd -> "Wall" cells, en détaillant les mouvements
-- Seulement pour les mouvements U/D (les R/L, pas besoin)
expand :: Pos -> Cmd -> Cmd -> [Cell]
expand (row, col) ("U", nb, color) (c, _, _) = [((row+i, col), ("U"++c, nb, color)) | i <- [-nb.. (-1)]]
expand (row, col) ("D", nb, color) (c, _, _) = [((row-i, col), ("D"++c, nb, color)) | i <- [-nb .. (-1)]]
expand (row, col) ("R", nb, color) (c, _, _) = [((row, col+nb), ("R"++c, nb, color))] --((row, col+i), color) | i <- [1..nb]]
expand (row, col) ("L", nb, color) (c, _, _) = [((row, col-nb), ("L"++c, nb, color))] --((row, col-i), color) | i <- [1..nb]]

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
