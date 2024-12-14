{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- import qualified MyParser as MP
import MyGrid ( GridMapCore, Pos, Size, DirVec, readGrid, chunks, allPos )
import Data.List ( groupBy, sort, group ) --, find ) --group ) sortOn, sort, , sortBy )
-- import Data.Tuple ( swap )
import Data.String.Utils ( join ) --, join, replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust )
-- import Control.Monad ( fold )
import qualified Data.Map as Map
import qualified Data.Set as Set
-- import Data.List ( sortOn )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
import Debug.Trace ( trace ) -- trace :: String > a -> a
traceThis :: (Show a) => a -> a
traceThis x = trace (show x) x
-- used as (1+2) `debug` "adding"'
debug = flip trace

-- import Numeric ( readHex )
-- import Control.Monad.State

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 21 Part - & -                                          **"
  putStrLn "********************************************************************************"
  -- content <- readFile "Input23/input21.txt"
  content <- readFile "Input23/test21_1.txt"

  let charGrid = readGrid (lines content)
  let posStart = fst $ head (Map.toList (Map.filter (== 'S') (snd charGrid)))
  print $ "posStart=" ++ show posStart

  -- let step1 = opExpand charGrid Set.empty posStart
  -- print $ "step1=" ++ show step1

  -- let maxNb = 64
  -- let steps = loopApply (maxNb, 0) (opExpand charGrid) (Set.singleton posStart)
  -- -- print $ "steps[" ++ show maxNb ++"]=" ++ show steps

  -- let pRes = Set.size (fst steps)
  -- putStrLn $ "Answer 1> " ++ show pRes

  -- let gs01 = genOpExtand charGrid ((0,0), Set.singleton posStart)
  -- print $ "gs01=" ++ show gs01
  -- let cl01 = cluster gs01
  -- print $ "cl01=" ++ show cl01

  let (rl, rr, cu, cd) = (-3, 3, -3, 3)
  let initGMap = (charGrid, Map.fromList [((0,0), Garden (0, 0) (Set.singleton posStart) Map.empty (-1) (-1) (-1) Map.empty) ])
  let initGMapW = (charGrid, Map.union (snd initGMap)
                                       (Map.fromList $ map (\p -> (p, mkGarden p))
                                                           (mkNetwork rl rr cu cd)))

  -- print $ "initGMap=" ++ show initGMap

  -- let next01 = gatherNewPos initGMap
  -- print $ "next01=" ++ show next01

  -- let gmap01 = updateGardens initGMap 1 (gatherNewPos initGMap)
  -- print $ "gmap01=" ++ show gmap01

  let nbStep = 70
  let newGarden = loopGardens (nbStep, 0) initGMapW -- (GardenMap, Int)
  let nbIteration = snd newGarden

  print $ "nbIteration=" ++ show nbIteration
  print $ "======Stopped at it=" ++ show nbIteration ++ "====="

  -- GardenMap -> Pos -> Int -> IO()
  let displayGarden gardenMap posGarden idxStep = do
        let garden = gardenMap Map.! posGarden
        let (one, two) = ((g_one garden), (g_two garden))
        let realIdx
              | idxStep <= one = idxStep
              | otherwise = one + (mod (idxStep - one) (two - one))

        let config = fst $ head $ Map.toList (Map.filter (== realIdx) (g_mem garden))
        print $ "-- MAP " ++ show posGarden
        print $ "(" ++ show one ++ ", " ++ show two ++ "  )"
        print $ "-- config after " ++ show idxStep ++ " iterations (same as " ++ show realIdx ++ ")"
        putStrLn (mapToGridString charGrid config)
        -- putStrLn ("frontier:\n" ++ displayFrontier (fst charGrid) (g_fron garden))

  let nbDisplay nbIter
        | nbIter == (nbStep -1) = nbIter
        | otherwise = nbIter - 2
  print $ "=== Showin " ++ show (nbDisplay nbIteration) ++ " iterations"
  print $ "  periodic ? " ++ show (areAllPeriodic (snd ( fst newGarden )))
  -- mapM_ (displayGarden (snd (fst newGarden)) (0, 0)) [0..(nbIteration - 1)]
  -- mapM_ (displayGarden (snd (fst newGarden)) (0, 0)) [0..(nbStep - 1)]
  mapM_ (displayGarden (snd (fst newGarden)) (0, 0)) [0..8]

  -- let allPosMap = Map.keys (snd (fst newGarden))
  -- print $ "allPosMap=" ++ show allPosMap

  let allPeriods = Map.foldrWithKey (\ k garden acc -> (k, (g_start garden, g_one garden, g_two garden)):acc ) [] (snd (fst newGarden))
  print $ "allPeriods=" ++ show allPeriods
  putStrLn $ chunks ((4+4+1)*(rr - rl + 1)) (displayPeriods allPeriods)

  let allScores = map scoreGarden (Map.elems (snd (fst newGarden)))
  -- print $ "allScores=" ++ show allScores
  putStrLn $ chunks ((4+1+3+1+4+1+4)*(rr -rl +1)) (concat $ map (displayScore . snd) allScores)
  -- putStrLn $ concat (map dumpScore allScores)

  let scoreMap = Map.fromList allScores

  let score9 = map (opScore scoreMap 9) (allPos (-2) 2 (-2) 2)
  print $ "score9=" ++ show score9
  print $ "sum9=" ++ show (sum (map snd score9))

  let score49 = map (opScore scoreMap 49) (allPos (-2) 2 (-2) 2)
  print $ "score49=" ++ show score49
  print $ "sum49=" ++ show (sum (map snd score49))
  print $ "  lineEast=" ++ show (repeatedLine 49 21 11)


  -- -- patterns in map ?
  -- let ((patMap, first, second), _) = loopPattern (150, 0) Map.empty (opExpand charGrid) (Set.singleton posStart)
  -- print $ "first=" ++ show first ++ " snd=" ++ show second
  -- -- print $ "patMap=" ++ show patMap

  -- let printSteps nb = do
  --       let repeatedPos = fst $ head $ (Map.toList (Map.filter (== nb) patMap))
  --       print $ "-- config after " ++ show (nb) ++ " iterations"
  --       putStrLn (mapToGridString (fst charGrid) repeatedPos)
  -- mapM_ printSteps [0..first]

  -- let repeatedPos0 = fst $ head $ (Map.toList (Map.filter (== (first-1)) patMap))
  -- print $ "-- config after " ++ show (first-1) ++ " iterations"
  -- putStrLn (mapToGridString (fst charGrid) repeatedPos0)
  -- let repeatedPos1 = fst $ head $ (Map.toList (Map.filter (== first) patMap))
  -- print $ "-- config after " ++ show first ++ " iterations"
  -- putStrLn (mapToGridString (fst charGrid) repeatedPos1)


  -- putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
-- 'I' is for staying in the same map in part 2
data Dir = N | W | S | E | I deriving (Eq, Ord, Show)

type GridMap = GridMapCore Char
-- type Pos = (Int, Int) -- (row, col)
type PosSet = Set.Set Pos

-- FIXME : devrait pas rester coincé au bord, mais ne pas donner de solution
-- MAIS Ok pour la question 1
gridStep :: (Int, Int) -> Pos -> Dir -> Pos
gridStep size      (r,c) N = (max 0 (r-1), c)
gridStep (rmax, _) (r,c) S = (min (rmax-1) (r+1), c)
gridStep size      (r,c) W = (r, max 0 (c-1))
gridStep (_, cmax) (r,c) E = (r, min (cmax-1) (c+1))

isValid :: GridMap -> Pos -> Bool
isValid (_, grid) pos = case Map.lookup pos grid of
  Just '#' -> False
  _        -> True

opExpand :: GridMap -> PosSet -> Pos -> PosSet
opExpand (size, grid) visited pos = Set.union visited extraPos
  where
    extraPos = Set.filter (isValid (size, grid)) (Set.fromList (map (gridStep size pos) [N, W, S, E]))

-- apply 'func' for a given nb of time (maxIt) or until empty input (maxIt < 0)
-- apply consume the head of input to produce newAcc given acc, using func
loopApply :: (Int, Int) -> (Set.Set a -> a -> Set.Set a) -> Set.Set a -> (Set.Set a, Int)
loopApply (maxIt, it) func input
  | Set.size input == 0 = (input, it)
  | maxIt < 0   = loopApply (maxIt, it+1) func newInput
  | it == maxIt = (input, it)
  | otherwise   = loopApply (maxIt, it+1) func newInput
  where
    newInput = Set.foldl func Set.empty input

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- Essayons de repérer des patterns
type PatternMap = Map.Map PosSet Int
-- type UpdatePosFunc = (PosSet -> Pos -> PosSet)

-- storePattern :: PatternMap -> PosSet -> Int -> (Maybe Int, PatternMap)
-- storePattern patMap config when = case Map.lookup config patMap of
--   Just nb -> (Just nb, patMap)
--   Nothing -> (Nothing, Map.insert config when patMap)

-- loopPattern :: (Int, Int) -> PatternMap -> UpdatePosFunc -> PosSet -> ((PatternMap, Int, Int), Int)
-- loopPattern (maxIt, it) patMap func input
--   | Set.size input == 0 = ((patMap, -1, -1), it)
--   | maxIt < 0   = foundPattern newInput
--   | it == maxIt = ((patMap, -2, -2), it)
--   | otherwise   = foundPattern newInput
--   where
--     newInput = Set.foldl func Set.empty input
--     foundPattern :: PosSet -> ((PatternMap, Int, Int), Int)
--     foundPattern config = case storePattern patMap config it of
--       (Just n, newPatMap)  -> ((newPatMap, n, it), it)
--       (Nothing, newPatMap) -> loopPattern (maxIt, it+1) newPatMap func config


-- *****************************************************************************
-- premieres sorties de particules ?
-- type Size = (Int, Int)
type Count = (Int, Int)
type FrontierMap = Map.Map Pos Int

data Garden = Garden { g_pos :: Pos          -- in the infinite pavement
                     , g_state :: PosSet     -- actual config
                     , g_mem :: PatternMap   -- store past config
                     , g_start :: Int        -- first entry time
                     , g_one :: Int          -- first time of pattern repetition
                     , g_two :: Int          -- second time pattern repeats
                     , g_fron :: FrontierMap } -- time of first visit on frontier
  deriving Show

type GardenMap = (GridMap, Map.Map Pos Garden)

mkGarden :: Pos -> Garden
mkGarden pos = Garden pos Set.empty Map.empty (-1) (-1) (-1) Map.empty

mkNetwork :: Int -> Int -> Int -> Int -> [Pos]
mkNetwork rLeft rRight cUp cDown = [(r, c) | r <- [rLeft .. rRight],
                                             c <- [cUp .. cDown],
                                             c /= 0 || r /= 0]

genStep :: Size -> Pos -> Pos -> Dir -> (Pos, Pos)
genStep (rmax, _) (gr, gc) (0, c) N = ((gr-1, gc), (rmax-1, c))
genStep _         (gr, gc) (r, c) N = ((gr, gc), (r-1, c))
genStep (rmax, _) (gr, gc) (r, c) S
  | r == (rmax-1) = ((gr+1, gc), (0, c))
  | otherwise     = ((gr, gc), (r+1, c))
genStep (_, cmax) (gr, gc) (r, 0) W = ((gr, gc-1), (r, cmax-1))
genStep _         (gr, gc) (r, c) W = ((gr, gc), (r, c-1))
genStep (_, cmax) (gr, gc) (r, c) E
  | c == (cmax-1) = ((gr, gc+1), (r, 0))
  | otherwise     = ((gr, gc), (r, c+1))

-- generate a list of [(posOfGarden, posInGarden)]
genOpExtand :: GridMap -> (Pos, PosSet) -> [(Pos, Pos)]
genOpExtand (size, grid) (posG, visited) = allNewPos
  where
    allNewPos = concat $ map newPos (Set.toList visited)
    newPos :: Pos -> [(Pos, Pos)]
    newPos pos = filter (isValid (size, grid) . snd) $ map (genStep size posG pos) [N, W, S, E]

-- gathers all new Pos from all Garden as [(gardenPos, Set of Pos for that gardent)]
cluster :: [(Pos, Pos)] -> [(Pos, PosSet)]
cluster lpp = map toSet grg
  where
    grg = groupBy (\(g1,_) (g2,_) -> g1 == g2) $ sort lpp
    toSet gGposPos = (fst ( head gGposPos ), Set.fromList (map snd gGposPos))

gatherNewPos :: GardenMap -> [(Pos, PosSet)]
gatherNewPos (gridMap, gardenMap) = cluster $ Map.foldr opGather [] gardenMap --traceThis $
  where
    opGather :: Garden -> [(Pos, Pos)] -> [(Pos, Pos)]
    opGather garden acc = acc ++ newPosPosAll
      where
        newPosPosAll = genOpExtand gridMap (g_pos garden, g_state garden)

updateGardens :: GardenMap -> Int -> [(Pos, PosSet)] -> GardenMap
updateGardens (gridmap, gmap) now [] = (gridmap, gmap)
updateGardens (gridmap, gmap) now ((posmap, posSet):pps) = updateGardens (gridmap, updatedGMap) now pps --`debug` ("--garden " ++ show posmap ++ "\n" ++ mapToGridString gridmap posSet)
  where
    updatedGMap :: Map.Map Pos Garden
    updatedGMap = case getGarden gmap posmap of
      Just gard ->  Map.adjust (const (upGarden (setStartTime (gard)))) posmap gmap -- `debug` ("updatedGMap upGarden=" ++ show (upGarden gard))
      Nothing ->  gmap -- `debug` ("no update")
    setStartTime :: Garden -> Garden
    setStartTime garden = if g_start garden < 0
                            then (garden {g_start = now})
                            else garden
    upGarden :: Garden -> Garden
    upGarden (Garden gpos gstate gmem start one two fron )= case Map.lookup posSet gmem of -- trace ("gpos =" ++ show gpos) $ c
      Just firstTime -> if two < 0
                           then Garden gpos posSet gmem start firstTime now fron
                           else Garden gpos posSet gmem start one two fron
      Nothing        -> Garden gpos posSet (Map.insert posSet now gmem) start one two (updateFrontier (fst gridmap) fron (Set.toList posSet) now)

getGarden :: Map.Map Pos Garden -> Pos -> Maybe Garden
getGarden gardenMap posmap = case Map.lookup posmap gardenMap of
  Just g ->  Just g -- `debug` ("Got garden " ++ show posmap)
  -- default : do not add Maps
  Nothing ->  Nothing -- `debug` ("No garden at " ++ show posmap)
  -- -- add only the map West to main
  -- Nothing -> if posmap == (0, -1)
  --               then Just (mkGarden posmap) `debug` "Adding garden (0, -1)"
  --               else Nothing


loopGardens :: Count -> GardenMap -> (GardenMap, Int)
loopGardens (maxIt, it) gmap
  | maxIt < 0   = nextGardens
  | it == maxIt = (gmap, it)
  | areAllPeriodic (snd gmap) = (gmap, it)
  | otherwise   = nextGardens
    where nextGardens = loopGardens (maxIt, it+1) (updateGardens gmap it $ gatherNewPos gmap)

areAllPeriodic :: Map.Map Pos Garden -> Bool
areAllPeriodic gardenMap = Map.foldr (\g b-> b && (g_one g >= 0) && (g_two g >= 0)) True gardenMap

-- from nb of Path to modulo ***************************************************
-- FIXME : added s_score_even, odd => to compute
data Score = Score { s_start :: Int              -- first visit
                   , s_one :: Int                -- period begins
                   , s_score_even :: Int         -- score for 'one + 2k'
                   , s_score_odd :: Int          -- score for 'one + 2k + 1'
                   , s_mem :: Map.Map Int Int }  -- idx -> score
  deriving Show
type ScoreMap = Map.Map Pos Score

displayScore :: Score -> String
displayScore (Score gstart gone geven godd _) = formatIntL 4 gstart ++ "/" ++ formatIntR 3 gone ++ "@" ++ formatIntL 4 geven ++ "|" ++ formatIntR 4 godd

dumpScore :: (Pos, Score) -> String
dumpScore (pos, (Score sstart sone seven sodd smem)) = show "Score: " ++ show pos
  ++ "\n  " ++ displayScore (Score sstart sone seven sodd smem)
  ++ "\n  =>" ++ (dumpScoreMem smem)
  ++ "\n"

dumpScoreMem :: Map.Map Int Int -> String
dumpScoreMem intMap = join ", " (map (\(k, a) -> show k ++ ":" ++ show a) (Map.toList intMap))

scoreGarden :: Garden -> (Pos, Score)
scoreGarden (Garden gpos _ gmem gstart gone _ _)
  | gone >= 0 = (gpos, Score gstart gone (fst scores) (snd scores) scoreMap)
  | otherwise = (gpos, Score gstart gone 0 0 scoreMap)
  where
    scores = getEvenOddScores gmem gone
    scoreMap = Map.fromList $ map (\(ps, t) -> (t, Set.size ps)) (Map.toList gmem)

-- extract the first (and normally only) PosSet associated with idx 'Time'
getPosSet :: PatternMap -> Int -> PosSet
getPosSet patternMap idx = fst $ head $ Map.toList (Map.filter (== idx) patternMap)

getEvenOddScores :: PatternMap -> Int -> (Int, Int)
getEvenOddScores patternMap firstTime
  | even firstTime = (scoreFirst, scoreOther)
  | otherwise      = (scoreOther, scoreFirst)
  where
    scoreFirst = Set.size $ getPosSet patternMap firstTime
    scoreOther = Set.size $ getPosSet patternMap (firstTime + 1)

-- Score from "central nodes"
opScore scoreMap nbStep pos
  | nbStep < sstart = (pos, 0)
  | nbStep < sone = (pos, smem Map.! nbStep)
  | even nbStep = (pos, seven)
  | otherwise = (pos, sodd)
  where Score sstart sone seven sodd smem = scoreMap Map.! pos

-- List of Garden reached in nbSteps
-- compute nb of time a TRUC is repeated in a line
repeatedLine :: Int -> Int -> Int -> (Int, Int)
repeatedLine nbStep first period
  | nb > 0 = (nb, leftover)
  | otherwise = (0, 0)
  where (nb, leftover) = divMod (nbStep - first) period


-- gardenScore :: ScoreMap -> Int -> Pos -> Int
-- gardenScore (gridmap, gardenMap) nbStep (gr, gc)
--   | abs gr < 3 && abs gc < 3 = getScore (scoreMap Map.! (gr, gc)) nbStep
--   | otherwise = getScoreAway (scoreMap Map.! (2 * signum gr, 2 * signum gc))

-- getScore :: Score -> Int -> Int
-- getScore (Score start one mem) idx = mem Map.! idxReal
--   where
--     idxReal idx
--       | nbStep > one = mod (nbStep - one) 2 + one
--       | otherwise    = start
-- getScoreAway (Score start one mem) idx =

-- -- Frontiers *******************************************************************
-- extract Frist visit time from Map.Map PosSet Int
updateFrontier :: Size -> FrontierMap -> [Pos] -> Int -> FrontierMap
updateFrontier _ frontier [] _ = frontier
updateFrontier (rmax, cmax) frontier (pos:ps) time
  | onFrontier pos = case Map.lookup pos frontier of
      Just n -> updateFrontier (rmax, cmax) frontier ps time
      Nothing -> updateFrontier (rmax, cmax) (Map.insert pos time frontier) ps time
  | otherwise = updateFrontier (rmax, cmax) frontier ps time
  where
    onFrontier :: Pos -> Bool
    onFrontier (r, c) = (r == 0) || (c == 0) || (r == rmax - 1) || (c == cmax - 1)

displayFrontier :: Size -> FrontierMap -> String
displayFrontier (rmax, cmax) frontier = fr_N ++ fr_W ++ fr_S ++ fr_E
  where
    listPosTime = Map.toList frontier
    fr_N = "  N: " ++ show (filter (\((r,c), _) -> r == 0) listPosTime) ++ "\n"
    fr_W = "  W: " ++ show (filter (\((r,c), _) -> c == 0) listPosTime) ++ "\n"
    fr_S = "  S: " ++ show (filter (\((r,c), _) -> r == (rmax - 1)) listPosTime) ++ "\n"
    fr_E = "  E: " ++ show (filter (\((r,c), _) -> c == (cmax - 1)) listPosTime) ++ "\n"

-- *****************************************************************************
-- display grid : repeat '.' sauf si coordonnées d'une position donnée par [Pos]

posToIdx :: (Int, Int) -> Pos -> Int
posToIdx (_, width) (r, c) = r * width + c
idxToPos :: (Int, Int) -> Int -> Pos
idxToPos (_, width) idx = divMod idx width

-- BEWARE : generate an INFINITE grid
genGrid :: GridMap -> Int -> [Pos] -> String
genGrid (size, grid) idx []
  | not (isValid (size, grid) (idxToPos size idx)) = '#' : genGrid (size, grid) (idx+1) []
  | otherwise                                      = '.' : genGrid (size, grid) (idx+1) []
genGrid (size, grid) idx (pos:ps)
  | idx == idPos = 'O' : genGrid (size, grid) (idx+1) ps
  | not (isValid (size, grid) (idxToPos size idx)) = '#' : genGrid (size, grid) (idx+1) (pos:ps)
  | otherwise    = '.' : genGrid (size, grid) (idx+1) (pos:ps)
  where idPos = posToIdx size pos

mapToGridString :: GridMap -> PosSet -> String
mapToGridString (size, grid) posSet = chunks (snd size)
                              $ take (fst size * snd size) (genGrid (size, grid) 0 (Set.toList posSet))

formatIntR :: Int -> Int -> String
formatIntR width nb
  | strLen < width = show nb ++ replicate (width - strLen) ' '
  | otherwise      = show nb
  where
    strLen = length (show nb)
formatIntL :: Int -> Int -> String
formatIntL width nb
  | strLen < width = replicate (width - strLen) ' ' ++ show nb
  | otherwise      = show nb
  where
    strLen = length (show nb)

displayPeriods :: [(Pos, (Int, Int, Int))] -> String
displayPeriods [] = ""
displayPeriods ((_, (start, period, next)):ps) = (formatIntL 4 start) ++ "/" ++ (formatIntR 4 period) ++ displayPeriods ps
