{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}

module Main where

-- seems correct answer is 1688 :o)

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid, chunks )
-- import MyGrid (Pos, Size, DirVec, GridMapCore,
--                readGrid, addDir, isValidPos, chunks, showGrid, getValMap, allDir)
-- import qualified MyCache as MC
-- ****** MyUtils: countTrue, groupLines

-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
import Control.Monad ( foldM )
-- import Control.Monad.Extra ( mapMaybeM )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
import Data.Maybe ( isNothing )
-- ****** Data.Foldable ( foldl' )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
import qualified Data.Map.Strict as Map

import qualified Data.Vector as VU
import qualified Data.Vector.Mutable as VUM
import Control.Monad.ST (runST)
-- import qualified Data.Set as Set
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl', elemIndex
-- ****** Data.List.Extra: splitOn
import Data.List ( intercalate )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import qualified Control.Monad.Trans.State as St
import qualified Control.Monad.State as St
-- import qualified Data.Bits as Bits

import System.Environment (getArgs)
-- import qualified Control.Monad.State as St

-- *********************************************************************************** DEBUG
-- import Debug.Trace ( trace ) -- trace :: String > a -> a
-- traceThis :: (Show a) => a -> a
-- traceThis x = trace (show x) x
-- -- used as (1+2) `debug` "adding"'
-- debug :: c -> String -> c
-- debug = flip trace

-- import qualified Text.Megaparsec            as P
-- import qualified Text.Megaparsec.Char       as P
-- import qualified Text.Megaparsec.Char.Lexer as PP
-- import Data.Void (Void)

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2025 - Day 11 Part - & -                                          **"
  putStrLn "********************************************************************************"

  args <- getArgs
  content <- case args of
        [fileName] -> readFile ("Input25/" ++ fileName ++ ".txt")
        _ -> error "Error: prog need <fileName>.txt"
  -- content <- readFile "Input21/inputxx.txt"
  -- content <- readFile "Input21/testxx_1.txt"

  let nMap = foldr parseNode Map.empty (lines content)
  putStrLn $ "nMap=" ++ show nMap
  -- let allPaths = stepPath nMap [] [["you"]]
  -- -- putStrLn $ "allPaths=" ++ show allPaths
  -- let pRes = length allPaths
  -- putStrLn $ "Answer 1> " ++ show pRes

  -- let allPathsAdv = stepPath nMap [] [["svr"]]
  -- let nbAdv = length allPathsAdv
  -- putStrLn $ "nbAdv=" ++ show nbAdv
  -- let goalF = stepPathMap nMap ((0,0,0,0), Map.singleton "svr" (1,0,0,0))
  -- putStrLn $ "goalF=" ++ show goalF

  -- -- GraphViz
  -- putStrLn "digraph Advent25_11 {"
  -- putStrLn $ unlines (toViz nMap)
  -- putStrLn "}"

  let pMap = invertMap nMap
  putStrLn $ "pMap" ++ show pMap
  let graph = mkNodeGraph pMap
  putStrLn $ "graph=" ++ show graph

  -- let nbSvrFft = countPath graph "svr" "fft" :: Int
  -- putStrLn $ "svr->fft: " ++ show ((countPath graph "svr" "fft") :: Int)
  -- putStrLn $ "fft->dac: " ++ show ((countPath graph "fft" "dac") :: Int)
  -- putStrLn $ "dac->out: " ++ show ((countPath graph "dac" "out") :: Int)
  let sol2 = go "fft" "dac" + go "dac" "fft"
        where
          go n1 n2 = product [ countPathMemo graph "svr" n1
                             , countPathMemo graph n1 n2
                             , countPathMemo graph n2 "out"
                             ]

  putStrLn $ "sol2=" ++ show sol2


  -- putStrLn $ "test=" ++ show (testFind nMap)
  -- putStrLn $ "Answer 2> " ++ show cRes


-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type NodeMap = Map.Map String [String]

parseNode :: String -> NodeMap -> NodeMap
parseNode line nMap = Map.insert idNode outNodes nMap
  where
    tok = words line
    idNode = init (head tok)
    outNodes = drop 1 tok

stepPath :: NodeMap -> [[String]] -> [[String]] -> [[String]]
stepPath _ paths [] = paths
stepPath nMap paths (o:os) = stepPath nMap newPaths newOpen
  where
    nexts = nMap Map.! (head o)
    (newPaths, newOpen) = foldr opNext (paths, os) nexts
    opNext :: String -> ([[String]], [[String]]) -> ([[String]], [[String]])
    opNext n (acc, open)
      | n == "out" = ((n:o) : acc, open)
      | otherwise = (acc, (n:o) : open)
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

-- *****************************************************************************
-- NOT WORKING -> not MUTABLE, so the number of pMaps fills the RAM
-- will store in a Node
-- nbPath to here without FFT or DAC
-- nbPath to here with ONLY FFT
-- nbPath to here with ONLY DAC
-- nbPath with both
type Memory = (Int, Int, Int, Int)
type PathMap = Map.Map String Memory


stepPathMap :: NodeMap -> (Memory, PathMap) -> Memory
stepPathMap nMap (goal, pMap)
  | Map.size pMap == 0 = goal
  | otherwise = stepPathMap nMap (foldr (arriveFrom memFrom) (goal, openMap) nexts)
    where
      (fromMap, openMap) = Map.splitAt 1 pMap
      keyFrom = head (Map.keys fromMap)
      memFrom = head (Map.elems fromMap)
      nexts = nMap Map.! keyFrom

arriveFrom :: Memory -> String -> (Memory, PathMap) -> (Memory, PathMap)
arriveFrom (fp, fof, fod, fb) "out" (goal, pMap) = ((gp+fp, gof+fof, god+fod, gb+fb), pMap)
  where (gp, gof, god, gb) = goal

arriveFrom (fp, fof, fod, fb) dest (goal, pMap) = case Map.lookup dest pMap of
  Nothing -> case dest of "fft" -> (goal, Map.insert dest (0, fof+fp, 0, fb+fod) pMap)
                          "dac" -> (goal, Map.insert dest (0, 0, fod+fp, fb+fof) pMap)
                          _     -> (goal, Map.insert dest (fp, fof, fod, fb) pMap)
  Just (dp, dof, dod, db) -> case dest of
                          "fft" -> (goal, Map.insert dest (0, dp+dof+fof+fp, 0, dod+db+fb+fod) pMap)
                          "dac" -> (goal, Map.insert dest (0, 0, dp+fp+fod+dod, fof+fb+dof+db) pMap)
                          _     -> (goal, Map.insert dest (dp+fp, fof+dof, fod+dod, fb+db) pMap)


-- *****************************************************************************
-- using Mutable of Maybe Memory
-- the index is given by the key index of the NodeMap
data VecMemory = M { m_id :: !String
                   , m_np :: !Int
                   , m_nf :: !Int
                   , m_nd :: !Int
                   , m_nb :: !Int }
instance Show VecMemory
  where show (M label np nf nd nb) = "["++ label ++"]"
                                  ++ show np ++ "/" ++ show nf ++ "/" ++ show nd ++ "/" ++ show nb
type VecContent = Maybe VecMemory
type PathCount s = VUM.MVector s VecContent

-- function to Initiate PathCount to Just (1, 0, 0, 0) for idx of "svr"
-- the idx of the "End" will be always be the last
-- and Nothing for all others
generateIdxValues
  :: NodeMap      -- NodeMap
  -> String       -- label of starting point (ie "svr")
  -> String       -- label of starting point (ie "out")
  -> Int          -- index of the [Mutable.] Vector
  -> VecContent -- RETURNS: value at the index
generateIdxValues nMap labelStart labelEnd idx
  | idx == idxStart = Just (M labelStart 1 0 0 0)
  | idx == idxEnd = Just (M labelEnd 0 0 0 0)
  | otherwise       = Nothing
  where
    idxStart = keyToIdx nMap labelStart
    idxEnd = Map.size nMap

-- read some vals
getVals :: (VUM.PrimMonad m, VUM.PrimState m ~ s)
  => PathCount s
  -> [Int]
  -> m [VecContent]
getVals pCount idxList = do
  foldM (\acc i -> do
                next <- VUM.read pCount i
                pure (next:acc)) [] idxList

-- find the first "non-goal" index that is still in the PathCount
-- findNextOpen
--   :: PathCount s
--   -> Maybe (idx, Memory)
-- findNextOpen :: VUM.MVector (VUM.PrimState m) (Maybe a1) -> m (Maybe a2)
-- findNextOpen :: VUM.MVector (VUM.PrimState []) VecContent -> f VecContent
findNextOpen :: VUM.PrimMonad m => VUM.MVector (VUM.PrimState m) VecContent -> m VecContent
findNextOpen pCount = do
  allOpenIdx <- mapM (VUM.read pCount) allIdx
  let openNotNothing = dropWhile isNothing allOpenIdx
  if null openNotNothing then pure Nothing
                                else pure (head openNotNothing)
  where
    allIdx = [0 .. VUM.length pCount - 2] -- the last one is for the Goal, not counting

-- turn 'key' to an Index
keyToIdx :: NodeMap -> String -> Int
keyToIdx nMap k = Map.findIndex k nMap

-- add a (.,.,.,)-path to a new (existing) node ?
{-
arriveFrom (fp, fof, fod, fb) dest (goal, pMap) = case Map.lookup dest pMap of
  Nothing -> case dest of "fft" -> (goal, Map.insert dest (0, fof+fp, 0, fb+fod) pMap)
                          "dac" -> (goal, Map.insert dest (0, 0, fod+fp, fb+fof) pMap)
                          _     -> (goal, Map.insert dest (fp, fof, fod, fb) pMap)
  Just (dp, dof, dod, db) -> case dest of
                          "fft" -> (goal, Map.insert dest (0, dp+dof+fof+fp, 0, dod+db+fb+fod) pMap)
                          "dac" -> (goal, Map.insert dest (0, 0, dp+fp+fod+dod, fof+fb+dof+db) pMap)
                          _     -> (goal, Map.insert dest (dp+fp, fof+dof, fod+dod, fb+db) pMap)
-}
-- updatePath pCpount (fp, fof, fod, fb)
updatePath :: VUM.PrimMonad m
  => Map.Map String [String]
  -> VUM.MVector (VUM.PrimState m) (Maybe VecMemory)
  -> VecMemory -> String
  -> m ()
updatePath nMap pCount (M _ fp ff fd fb) "out" = do
  let iDest = Map.size nMap
  prevMemory <- VUM.read pCount iDest
  case prevMemory of
    Just (M _ tp tf td tb) -> VUM.write pCount iDest (Just $ M dest (tp+fp) (tf+ff)
                                                                    (td+fd) (tb+fb))
    Nothing                -> VUM.write pCount iDest (Just $ M dest fp ff fd fb)
  where
    dest = "out"
updatePath nMap pCount (M _ fp ff fd fb) "fft" = do
  let iDest = keyToIdx nMap dest
  prevMemory <- VUM.read pCount iDest
  case prevMemory of
    Just (M _ _ tf _ tb) -> VUM.write pCount iDest (Just $ M dest 0 (tf+fp+ff)
                                                                    0 (tb+fd+fb))
    Nothing                -> VUM.write pCount iDest (Just $ M dest 0 (fp+ff) 0 (fd+fb))
  where
    dest = "fft"
updatePath nMap pCount (M _ fp ff fd fb) "dac" = do
  let iDest = keyToIdx nMap dest
  prevMemory <- VUM.read pCount iDest
  case prevMemory of
    Just (M _ _ _ td tb) -> VUM.write pCount iDest (Just $ M dest 0 0 (td+fp+fd)
                                                                      (tb+ff+fb))
    Nothing                -> VUM.write pCount iDest (Just $ M dest 0 0 (fp+fd) (ff+fb))
  where
    dest = "dac"
updatePath nMap pCount (M _ fp ff fd fb) dest = do
  let iDest = keyToIdx nMap dest
  prevMemory <- VUM.read pCount iDest
  case prevMemory of
    Just (M _ tp tf td tb) -> VUM.write pCount iDest (Just $ M dest (tp+fp) (tf+ff) (td+fd) (tb+fb))
    Nothing                -> VUM.write pCount iDest (Just $ M dest fp ff fd fb)

diffusePath nMap pCount = do
  nFrom <- findNextOpen pCount
  case nFrom of
    Nothing -> do
      VUM.read pCount idxGoal
      where
        idxGoal = Map.size nMap
    Just (M label fp ff fd fb) -> do
        VUM.write pCount idxFrom Nothing
        mapM_ (updatePath nMap pCount (M label fp ff fd fb)) nextNodes
        pure Nothing
        where
          idxFrom = keyToIdx nMap label
          nextNodes = nMap Map.! label


-- ****************************************************************************
-- some testing
-- testFind :: [VecContent]
testFind nMap = runST $ do

  let nbLabel = Map.size nMap
  let vinit = VU.generate (nbLabel+1) (generateIdxValues nMap "svr" "out")
  -- create and init mutable Vector
  pCount <- VUM.new (nbLabel+1)
  mapM_ (\i -> VUM.write pCount i (vinit VU.! i)) [0 .. nbLabel]

  -- try to implement a kind of untilJust
  recDiffuse pCount
    where
      recDiffuse mutVec= do
        x <- diffusePath nMap mutVec
        case x of
          Nothing -> recDiffuse mutVec
          Just res  -> return res
  -- diffusePath nMap pCount
  -- diffusePath nMap pCount
  -- diffusePath nMap pCount
  -- add dest "aaa"
  -- updatePath nMap pCount (M "svr" 1 0 1 0) "aaa"
  -- updatePath nMap pCount (M "svr" 2 0 0 0) "fft"
  -- updatePath nMap pCount (M "svr" 3 1 1 4) "fft"
  -- updatePath nMap pCount (M "svr" 1 2 3 4) "out"
  -- -- read
  -- getVals pCount [0 .. nbLabel]
  -- -- find an "Open" case in pCount
  -- VUM.write pCount (keyToIdx nMap "svr") Nothing
  -- -- -- read
  -- -- getVals pCount [0 .. nbLabel]
  -- findNextOpen pCount

  -- where
  --   nbLabel = Map.size nMap
  --   vinit = VU.generate (nbLabel+1) (generateIdxValues nMap "svr" "out")

-- ************************************************************* SMART WARNING
{- Check that there are no cycle in graph, by using Graphviz/DOT
   => ok, ny cycle, just large graph

   We will need some caching.
-}
-- to GraphViz
nodeViz label = "node [shape=box color=\""++ colStr ++"\"] " ++ label ++ ";"
  where
    colStr = if elem label ["svr", "fft", "dac", "out"] then "red" else "black"

edgeViz fLabel tLabel = fLabel ++ " -> " ++ tLabel ++ ";"

toViz nMap = map nodeViz ("out" : Map.keys nMap)
             ++ concatMap (\(k, v) -> map (edgeViz k) v) (Map.toList nMap)

-- first invert NodeMap into a list of (child, [list_of_parent])
invertMap :: NodeMap -> [(String, [String])]
invertMap nMap = groupedByChild
  where
    kChildList = Map.foldrWithKey opToPairList [] nMap
    -- pairs of (child, parent)
    opToPairList key children acc = [(child, key) | child <- children] ++ acc
    -- now invert
    groupedByChild = map (\child -> (child, map snd $ filter ((==child) . fst) kChildList))
                         ("out" : Map.keys nMap)

-- then, we can build a Recursive Structure with Node
type GraphMap = Map.Map String Node
data Node = Node { n_name :: String
                 , n_parents :: [Node] }
instance Show Node where
  show (Node name parents) = concat
    [ "[" ++ name ++ " <- "
    , intercalate "/" (map n_name parents)
    , "] "
    ]

-- the 'trick' is "Tying the Knot": use lazyness and use a 'dictionnary' of Nodes
-- @see: https://wiki.haskell.org/Tying_the_Knot
mkNodeGraph :: [(String, [String])] -> GraphMap
mkNodeGraph childParentList = graphMap
  where
    graphMap = Map.fromList $
      map (\(child, parents) -> (child, Node child [graphMap Map.! e | e <- parents]))
          childParentList

-- then, can use Traversable property of Node
countPath graph fromName toName = visit fromName (graph Map.! toName)
visit :: String -> Node -> Int
visit fromName node =
  if n_name node == fromName
    then 1
    else
      sum (map (visit fromName) (n_parents node))

-- BUT, too long => need to cache result (in a Map, for example)

--visitMemo :: String -> Node -> Int
-- visitMemo fromName node sMap = do
--   if n_name node == fromName
--     then pure 1
--     else do
--       prev <- St.gets (Map.lookup (n_name node) )
--       case St.gets (Map.lookup (n_name node)) of
--         Just result -> pure result
--         Nothing -> do
--           result <- fmap sum (traverse (visitMemo fromName) (n_parents node))
--           St.modify $ Map.insert (n_name node) result
--           pure result

-- TODO: do not work if not {-# LANGUAGE FlexibleContexts #-}
-- @see also: How do I assert the type of an inner function depends on the
--            type of the outer function in Haskell?
-- https://stackoverflow.com/questions/52786263/how-do-i-assert-the-type-of-an-inner-function-depends-on-the-type-of-the-outer-f
-- countPathMemo :: GraphMap -> String -> String -> Maybe Int
countPathMemo :: GraphMap -> String -> String -> Int
countPathMemo graph fromName toName =
  flip St.evalState Map.empty $ visitMemo (graph Map.! toName)
  where
    -- TODO do not work without FlexibleContext
    -- need to put this precise type
    visitMemo :: (Monad m) => Node -> St.StateT (Map.Map String Int) m Int
    visitMemo node = do
      if n_name node == fromName
        then pure 1
        else do
          prev <- St.gets (Map.lookup (n_name node))
          -- case St.gets (Map.lookup (n_name node)) of
          case prev of
            Just result -> pure result
            Nothing -> do
              result <- fmap sum (traverse visitMemo (n_parents node))
              St.modify (Map.insert (n_name node) result)
              pure result

-- TODO not without FlexibleContent
{-• Non type-variable argument
        in the constraint: St.MonadState (Map.Map String a) f
    • In the type signature:
        visitMemoA :: (Num a, St.MonadState (Map.Map String a) f) =>
                      String -> Node -> f a
    Suggested fix: Perhaps you intended to use FlexibleContexts
-}
-- visitMemoA :: (Num a, St.MonadState (Map.Map String a) f) => String -> Node -> f a
-- visitMemoA :: (St.MonadState (Map.Map String Int) f) => String -> Node -> f Int
-- => solution is the following type
-- visitMemoA :: (Monad m) => String -> Node -> St.StateT (Map.Map String Int) m Int
-- visitMemoA fromName node = do
--   if n_name node == fromName
--     then pure 1
--     else do
--     prev <- St.gets (Map.lookup (n_name node))
--     -- case St.gets (Map.lookup (n_name node)) of
--     case prev of
--       Just result -> pure result
--       Nothing -> do
--         result <- fmap sum (traverse (visitMemoA fromName) (n_parents node))
--         St.modify (Map.insert (n_name node) result)
--         pure result
