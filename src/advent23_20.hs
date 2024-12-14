{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
module Main where

-- import qualified MyParser as MP
-- import MyParser ( GridMap, readGrid )
-- import Data.List ( sortOn, groupBy ) --, find ) --group ) --, sort, , sortBy )
-- import Data.Tuple ( swap )
import Data.String.Utils ( split, join ) --replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust )
-- import Control.Monad ( fold )
import qualified Data.Map as Map
-- import Data.List ( sortOn )
-- import Data.Time.Clock.POSIX ( getPOSIXTime )
-- import Data.Char ( ord )
-- import Debug.Trace ( trace )
-- import Numeric ( readHex )
-- import Control.Monad.State

ff1 = FlipFlop Low ["a", "b"]
ffs1 = (Low, ["a", "b"])

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 20 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input20.txt"
  -- content <- readFile "Input23/test20_1.txt"
  -- content <- readFile "Input23/test20_2.txt"

  let nodes = map parseLine (lines content)
  let nodeMap = Map.fromList nodes
  let fullNodeMap = foldl parseForSources nodeMap (lines content)
  -- print $ "nodeMap=" ++ show nodeMap
  -- print $ "fullNodeMap=" ++ show fullNodeMap

  -- let steps1 = deliver [] fullNodeMap [("button", Low, "broadcaster")]
  -- print $ "steps1=" ++ show steps1

  -- let debugSteps nb = do
  --       let steps = deliverD (nb,0) [] fullNodeMap [("button", Low, "broadcaster")]
  --       print $ "-------- nb=" ++ show nb
  --       print steps
  -- mapM_ debugSteps [0..12]

  -- let (sent1, _, _) = steps1
  -- let cPulses = countPulse sent1
  -- print $ "cPulses=" ++ show cPulses

  -- Part 1
  let res = foldr opPush ([], fullNodeMap) [1..1000]
  -- print $ "res=" ++ show res

  let (counts, _) = res
  let (totLow, totHigh) = foldr (\(l,h) (sumL,sumH) -> (l+sumL, h+sumH)) (0,0) counts
  putStrLn $ "Answer 1> " ++ show (totLow * totHigh)

  -- let debugSpecial nb = do
  --       -- let steps = deliverD (nb, 0) [] fullNodeMap [("button", Low, "broadcaster")]
  --       let (steps, newMap) = opPushSpecial 0 ([], fullNodeMap)
  --       let withRx = filter rxSignal steps
  --       print "-- PushSpecial "
  --       print $ "steps=" ++ show steps


  --       print $ "withRx=" ++ show withRx
  -- debugSpecial 0

  -- Naive !! let cRes = pushSpecial 0 fullNodeMap

  -- putStrLn $ toGraph fullNodeMap

  let (qh1, qhMap1, sigQh1) = countPush 1 (isHigh "qh") fullNodeMap
  print $ "qh1=" ++ show qh1

  let (xm1, xmMap1, sigXm1) = countPush 1 (isHigh "xm") fullNodeMap
  print $ "xm1=" ++ show xm1

  let (pv1, pvMap1, sigPv1) = countPush 1 (isHigh "pv") fullNodeMap
  print $ "pv1=" ++ show pv1

  let (hz1, hzMap1, sigHz1) = countPush 1 (isHigh "hz") fullNodeMap
  print $ "hz1=" ++ show hz1

  let cRes = lcm qh1 (lcm pv1 (lcm hz1 xm1))
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"
-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
data Pulse = Low | High deriving (Show, Eq)
-- signal (src, pulse, dest)
type Signal = (String, Pulse, String)

type NodeMap = Map.Map String Node

-- FlipFlop ********************************************************************
data Node = FlipFlop  { mem :: Pulse , dest :: [String] }
          | Conjunction { mems :: Map.Map String Pulse , links :: [String] }
          | BroadCaster { bDest :: [String] }

-- data FlipFlop = FlipFlop { mem :: Pulse
--                          , dest :: [String] }
instance Show Node where
  show (FlipFlop mem dest) = "FF_" ++ show mem ++ " @" ++ show dest
  show (Conjunction mems links) = "CO_" ++ show mems ++ " @" ++ show links
  show (BroadCaster dest) = "BR_" ++ show dest

sendAll :: String -> Pulse -> [String] -> [Signal]
sendAll src pulse dest = map (\d -> (src, pulse, d)) dest

-- Conjunction *****************************************************************
-- data Conjunction = CO { mems :: Map.Map String Pulse
--                       , links :: [String] }
-- instance Show Conjunction where
--   show co = "CO_" ++ show (mems co) ++ " @" ++ show (links co)

initConjunction :: [String] -> Node
initConjunction dest = Conjunction Map.empty dest

addConjunctionSrc :: Node -> String -> Node
addConjunctionSrc (Conjunction mems links) src = Conjunction addedMap links
  where addedMap = Map.insert src Low mems
addConjunctionSrc node _ = node

runNode :: String -> Node -> Signal -> (Node, [Signal])
runNode nodeName (FlipFlop mem dest) (_, High, _) = (FlipFlop mem dest, [])
runNode nodeName (FlipFlop mem dest) (_, Low, _)
  | mem == Low = (FlipFlop High dest, sendAll nodeName High dest)
  | otherwise  = (FlipFlop Low  dest, sendAll nodeName Low dest)
-- runConjunction :: Conjunction -> Signal ->(Conjunction, [Signal])
runNode nodeName (Conjunction mems links) (src, pulse, dest)
  | allHigh   = (Conjunction updatedMap links, sendAll nodeName Low links)
  | otherwise = (Conjunction updatedMap links, sendAll nodeName High links)
  where
    updatedMap = Map.adjust (const pulse) src mems
    allHigh = Map.foldr (\p acc -> p == High && acc) True updatedMap
-- runBroadcaster :: BroadCaster -> Signal -> (BroadCaster, [Signal])
runNode nodeName (BroadCaster bDest) (_, pulse, _) = (BroadCaster bDest, sendAll nodeName pulse bDest)

-- some nodes (like "output") are just sinkholes
-- change Node and gather new signals
deliverSignal :: NodeMap -> Signal -> (NodeMap, [Signal])
deliverSignal nodeMap (from, pulse, to) = case nodeMap Map.!? to of
    Just node -> (Map.adjust (const (fst res)) to nodeMap, newSignals)
      where
        res = runNode to node (from, pulse, to)
        newSignals = snd res
    Nothing   -> (nodeMap, [])

deliver :: [Signal] -> NodeMap -> [Signal] -> ([Signal], NodeMap, [Signal])
deliver sent nodeMap [] = (sent, nodeMap, [])
deliver sent nodeMap (si:sis) = deliver (si:sent) newMap newSignals
  where
    (newMap, extraSignals) = deliverSignal nodeMap si
    newSignals = sis ++ extraSignals

deliverD :: (Int, Int) -> [Signal] -> NodeMap -> [Signal] -> ([Signal], NodeMap, [Signal])
deliverD (maxIt, curIt) sent nodeMap [] = (sent, nodeMap, [])
deliverD (maxIt, curIt) sent nodeMap (si:sis)
  | curIt == maxIt = (sent, nodeMap, si:sis)
  | otherwise = deliverD (maxIt, curIt+1) (si:sent) newMap newSignals
  where
    (newMap, extraSignals) = deliverSignal nodeMap si
    newSignals = sis ++ extraSignals

countPulse :: [Signal] -> (Int, Int)
countPulse signals = (nbLow, nbHigh)
  where
    nbLow = length (filter (\(_, p, _) -> p == Low) signals)
    nbHigh = length signals - nbLow

opPush :: Int -> ([(Int,Int)], NodeMap) -> ([(Int,Int)], NodeMap)
opPush episode (count, nodeMap) = ((countPulse sent):count, newNodeMap)
  where
    (sent, newNodeMap, _) = deliver [] nodeMap [("button", Low, "broadcaster")]

-- -- ***************** USING STATE ???? ******************************************
-- type FlipFlopState = (Pulse, [String])
-- showS :: FlipFlopState -> String
-- showS ffs = "FF*_" ++ show (fst ffs) ++ " @" ++ show (snd ffs)

-- runFlipFlopS :: Pulse -> State FlipFlopState [(String, Pulse)]
-- runFlipFlopS High = return []
-- runFlipFlopS Low = do
--   (mem, dest) <- get
--   case mem of
--     Low -> do
--       put (High, dest)
--       return (map (\d -> (d, High)) dest)
--     High -> do
--       put (Low, dest)
--       return (map (\d -> (d, Low)) dest)

-- Parsing *********************************************************************

parseLine :: String -> (String, Node)
parseLine (l:ls)
  | l == '%' = (name, FlipFlop Low dest)
  | l == '&' = (name, initConjunction dest)
  | otherwise = ("broadcaster", BroadCaster dest)
  where (name, dest) = parseArrow ls

parseArrow :: String -> (String, [String])
parseArrow str = (name, dest)
  where
    tokens = split " -> " str
    dest = split ", " (head (drop 1 tokens))
    name = head tokens

parseForSources :: NodeMap -> String -> NodeMap
parseForSources nodeMap (l:ls)
  | l == '%' = foldr (addSrc name) nodeMap dest
  | l == '&' = foldr (addSrc name) nodeMap dest
  | otherwise = foldr (addSrc "broadcaster") nodeMap dest
  where
    (name, dest) = parseArrow ls
    addSrc :: String -> String -> NodeMap -> NodeMap
    addSrc srcName destName nm = Map.adjustWithKey (\k n -> addConjunctionSrc n srcName) destName nm

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
rxSignal :: Signal -> Bool
rxSignal (_, Low, "rx") = True
rxSignal _ = False

-- stop when (_, Low, "rx") signal is delivered
specialDeliver :: NodeMap -> [Signal] -> (Bool, NodeMap, [Signal])
specialDeliver nodeMap [] = (False, nodeMap, [])
specialDeliver nodeMap (si:sis)
  | rxSignal si = (True, nodeMap, sis)
  | otherwise = specialDeliver newMap newSignals
  where
    (newMap, extraSignals) = deliverSignal nodeMap si
    newSignals = sis ++ extraSignals

pushSpecial :: Int -> NodeMap -> Int
pushSpecial curNb nodeMap
  | rxOk = curNb
  | otherwise = pushSpecial (curNb+1) newNodeMap
  where
    (rxOk, newNodeMap, _) = specialDeliver nodeMap [("button", Low, "broadcaster")]

opPushSpecial :: Int -> ([Signal], NodeMap) -> ([Signal], NodeMap)
opPushSpecial episode (count, nodeMap) = (sent, newNodeMap)
  where
    (sent, newNodeMap, _) = deliver [] nodeMap [("button", Low, "broadcaster")]

-- *****************************************************************************
-- plot graph to GraphViz
-- *****************************************************************************

toGraphvizNode :: String -> Node -> String
toGraphvizNode name (FlipFlop _ _) = name ++ "[shape=box]"
toGraphvizNode name (Conjunction _ _) = name ++ "[shape=circle]"
toGraphvizNode name (BroadCaster _) = name ++ "[shape=polygon]"

toGraphvizEdge :: String -> Node -> String
toGraphvizEdge name (FlipFlop _ dest) = name ++ " -> {" ++ (join " " dest) ++ "}"
toGraphvizEdge name (Conjunction _ links) = name ++ " -> {" ++ (join " " links) ++ "}"
toGraphvizEdge name (BroadCaster links) = name ++ " -> {" ++ (join " " links) ++ "}"

toGraph :: NodeMap -> String
toGraph nodeMap = "digraph G { {\n" ++ (join "\n" nodeDesc) ++ "\nrx [shape=star]" ++ "\n}\n" ++ (join "\n" nodeEdge) ++ "\n}\n"
  where
    nodeDesc = map (\(k, n) -> toGraphvizNode k n) (Map.toList nodeMap)
    nodeEdge = map (\(k, n) -> toGraphvizEdge k n) (Map.toList nodeMap)

-- Draw in GraphViz
{-
rx <- kh : CO (qh, pv, hz, xm) => Low si tous High
qh <- CO (fl) => qh High si fl Low 3761
pv <- CO (tb) => pv High si tb Low 4049
hz <- CO (hd) => hz High si hd Low 4079
xm <- CO (kc) => xm High si kc Low 3931 + 3592 => 7423

essayons de chercher les périodes de fl, tb, hd, kc => 244178746156661
-}
isLow :: String -> Signal -> Bool
isLow src (from, Low, to) = from == src
isLow _ _ = False
isHigh :: String -> Signal -> Bool
isHigh src (from, High, to) = from == src
isHigh _ _ = False

countPush :: Int -> (Signal -> Bool) -> NodeMap -> (Int, NodeMap, [Signal])
countPush curNb okSignal nodeMap
  | sigOk = (curNb, nodeMap, sent)
  | otherwise = countPush (curNb+1) okSignal newNodeMap
  where
    (sent, newNodeMap, _) = deliver [] nodeMap [("button", Low, "broadcaster")]
    sigOk = any okSignal sent
