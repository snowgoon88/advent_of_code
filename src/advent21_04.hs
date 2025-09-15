{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore,
--                 readGrid, addDir, isValidPos, chunks, showGrid, getValMap, allDir)
import qualified MyGrid as MG
-- import qualified MyCache as MC
-- ****** MyUtils: countTrue, groupLines
import qualified MyUtils as MU

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
import Data.String.Utils (split)
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
import qualified Data.Map as Map
-- import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl'
import Data.List ((\\), intersect)
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

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2021 - Day 04 Part - & -                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input21/input04.txt"
  -- content <- readFile "Input21/test04_1.txt"

  let contentGroup = MU.groupLines (lines content)
  let nbs = parseNbs (head (head contentGroup))
  -- putStrLn $ "nbs=" ++ show nbs

  let bMaps = parseGroup (drop 1 contentGroup)
  -- putStrLn $ "bMaps=" ++ show bMaps

  let posMap = makePosMap bMaps
  -- putStrLn $ "posMap=" ++ show posMap

  let (pRes, finalBMaps) = stepDraw posMap bMaps nbs
  -- putStrLn $ "test_nbs= " ++ show test_nbs
  -- putStrLn $ "\n ------- MAPS ----- \n" ++ nicebMap finalBMaps
  putStrLn $ "Answer 1> " ++ show pRes

  let resLost = nextDraw posMap bMaps ([], Map.keys bMaps) 0 nbs
  putStrLn $ "resLost=" ++ show resLost
  let (_, (_, cRes)) = last (fst resLost)
  putStrLn $ "Answer 2> " ++ show cRes


  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type ValMap = MG.GridMapCore Int               -- Map with Values
type BingoMap = Map.Map Int (ValMap, [MG.Pos]) -- m -> (Board, list token placed)
type PosMap = Map.Map Int [(Int, MG.Pos)]        -- Val -> list of (Board, Pos)

parseNbs :: String -> [Int]
parseNbs line = map read (split "," line)

parseBingoMap :: [String] -> ValMap
parseBingoMap linesG = ((length linesG, length (words (head linesG))),
                        Map.fromList [((row, col), read c) |
                                      (row, line) <- zip [0..] linesG,
                                      (col, c) <- zip [0..] (words line)])

parseGroup :: [[String]] -> BingoMap
parseGroup group = Map.fromList [(m, (parseBingoMap ls, [])) | (m, ls) <- zip [0..] group]

makePosMap :: BingoMap -> PosMap
makePosMap bMaps = Map.unionsWith (++) (map (\(m, (valMap, _)) -> valToPosMap m valMap) (Map.toList bMaps))
valToPosMap :: Int -> ValMap -> PosMap
valToPosMap m (_, vMap) = Map.fromList [(a, [(m, k)]) | (k, a) <- Map.toList vMap]

-- stepDraw ::
--   PosMap                   -- map v => list of (idx board, pos)
--   -> BingoMap              -- boards and played tokens
--   -> [Int]                 -- list of values to draw
--   -> (Maybe Int, BingoMap)
stepDraw _ bMaps [] = (Nothing, bMaps)
-- stepDraw :: PosMap -> BingoMap -> Int -> (Maybe Int, BingoMap)
stepDraw posMap bMaps (v:vs) = case res of
  Nothing -> stepDraw posMap newBMaps vs
  Just finalScore -> (Just finalScore, newBMaps)
  where
    posList = posMap Map.! v
    (res, newBMaps) = opDraw posList (Nothing, bMaps)

    opDraw :: [(Int, MG.Pos)] -> (Maybe Int, BingoMap) -> (Maybe Int, BingoMap)
    opDraw [] (_, bmap) = (Nothing, bmap)
    opDraw ((m, pos):ps) (_, bmap) = case resPut of
      Nothing -> opDraw ps (Nothing, newbmap)
      Just score -> (Just (v * score), newbmap)
      where
        (resPut, newbmap) = putAndCheckWin (m, pos) bmap

-- put a Token en check win (Nothing, Just score)
putAndCheckWin ::
  (Int, MG.Pos)             -- (map idx, pos of token)
  -> BingoMap               -- map of Boards and played tokens
  -> (Maybe Int, BingoMap)  -- Just score in win
putAndCheckWin (m, pos) bingoMap = if winning
                                     then (Just $ scoreBoard (vMap, newPlayed), newBingoMap)
                                     else (Nothing, newBingoMap)
  where
    (vMap, listPlayed) = bingoMap Map.! m
    newPlayed = pos : listPlayed
    newBingoMap = Map.insert m (vMap, newPlayed) bingoMap
    winning = checkWin pos (vMap, newPlayed)

-- check if a board is a Winner WHEN a token is put at pos
checkWin ::
  MG.Pos        -- position where a token has been added
  -> (ValMap, [MG.Pos])   -- board with a list of playedPos
  -> Bool
checkWin (row, col) (((nbRow, nbCol), _), playedPos) = checkCol || checkRow
  where
    checkCol = length (intersect playedPos [(row,c) | c <- [0..nbCol-1]]) == nbCol
    checkRow = length (intersect playedPos [(r,col) | r <- [0..nbRow-1]]) == nbRow

-- sum the value of the board not played
scoreBoard :: (ValMap, [MG.Pos]) -> Int
scoreBoard ((_, vMap), playedPos) = sum (Map.elems filteredMap)
  where
    filteredMap = Map.filterWithKey (\k _ -> notElem k playedPos) vMap

nicebMap :: BingoMap -> String
nicebMap bMaps = concatMap showNiceBingoMap (Map.toList bMaps)
  where
    showNiceBingoMap :: (Int, (ValMap, [MG.Pos])) -> String
    showNiceBingoMap (m, (vMap, posList)) = "Board (" ++ show m ++
      ") : " ++ show (take 5 $ (Map.toList . snd) vMap) ++
      "\n => " ++ show posList ++ "\n"

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- build list of (idx map, nb_steps_to_victory)
-- while removing the winning lists from the list of board yet to win
-- nextDraw
--   :: PosMap
--   -> BingoMap
--   -> ([(Int, (Int, Int))], [Int])  -- [ (idx bMap, (step to win, score)) ] x [idx bMap]
--   -> Int                           -- nb step so far
--   -> [Int]                         -- tokens draw
--   -> ((Int, (Int, Int)), [Int])    -- winning map, step, score AND map not winning
nextDraw _ bMaps (won, still) _ [] = (won, still)
nextDraw _ bMaps (won, []) _ _ = (won, [])
nextDraw posMap bMaps (won, stillToWin) n (v:vs) =
  nextDraw posMap newBMap (won ++ wins, newStillToWin) (n+1) vs
  where
    posList = posMap Map.! v
    -- remove pos in board that won
    tryList = filter (\(m, _) -> notElem m (map fst won)) posList
    (wins, newBMap) = opDraw tryList ([], bMaps)
    newStillToWin = stillToWin \\ map fst wins

    opDraw [] (acc, bmap) = (acc, bmap)
    opDraw ((m, pos):ps) (acc, bmap) = case resPut of
      Nothing -> opDraw ps (acc, newbmap)
      Just score -> opDraw ps ((m, (n, v * score)):acc, newbmap)
      where
        (resPut, newbmap) = putAndCheckWin (m, pos) bmap
