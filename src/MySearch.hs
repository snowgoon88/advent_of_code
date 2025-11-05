module MySearch where

{- A* search aglorithm.

import MySearch ( aStar )
aStar isGoal hDist getNeighbors startNode
-}

import Numeric.Limits ( maxValue )
import Data.List ( sortOn )
import qualified Data.Map as Map

-- A* **************************************************************************
-- Need to store in openSet: previous, score so far
data AstarNode a b = ASN { gScore    :: b        -- score of the path so far
                         , fScore   :: b         -- guess total score to goal
                         , previous :: Maybe a } -- parent of current node
  deriving (Show)

-- Memory is made of a Map: node -> (gScore, fScore, previous)
-- and a default value for a node (computed by aStarIni)
type AstarMap a b = Map.Map a (AstarNode a b)
type AstarMemory a b = (AstarMap a b, AstarNode a b)

-- | aStar : compute best path using A*
-- aStar isGoal hDist getNeighbors startNode
aStar :: (RealFloat b, Foldable t, Ord a)
  => (a -> Bool)      -- isGoal ?
  -> (a -> b)         -- hDist
  -> (a -> t (b, a))  -- getNeighbors
  -> a                -- startNode
  -> Maybe ([a], AstarMap a b)  -- Returns: Maybe( endNode, memory )
aStar isGoal hDist getNeighbors startNode = case bestSol of
  Nothing -> Nothing
  Just (endNode, finalMemory) -> Just ( aStarReconstructPath finalMemory [endNode]
                                      , finalMemory)
  where
    initMemory = aStarInit hDist startNode
    bestSol = aStarGo initMemory isGoal hDist getNeighbors [startNode]


-- | Initialize aStar Memory
aStarInit :: (Ord a, RealFloat b)
  => (a -> b)        -- heuristic distance function
  -> a               -- node/state of graph
  -> AstarMemory a b -- Returns: (memory of aStar, defaultMemoryValue)
aStarInit hDist start = ( Map.insert start (ASN 0 (hDist start) Nothing) Map.empty
                        -- default value for AStar Memory element
                        -- (i.e: gScore and fScore are set to infinity)
                        , ASN maxValue maxValue Nothing )

-- | Steps of A* until openSet is empty => error or isGoal current is True
aStarGo :: (Foldable t, Num b, Ord b, Ord a)
  => AstarMemory a b     -- memory (genereated by aStarInit)
  -> (a -> Bool)         -- isGoal ?
  -> (a -> b)            -- hDist
  -> (a -> t (b, a))     -- getNeighbors
  -> [a]                 -- current openSet
  -> Maybe (a, AstarMap a b)  -- Returns: Maybe (finalNode, A* map as memory)
aStarGo _        _      _     _            [] = Nothing
aStarGo (asMap, defVal) isGoal hDist getNeighbors openSet
  | isGoal current = Just (current, asMap)
  | otherwise = aStarGo (newAsMap, defVal) isGoal hDist getNeighbors newNext
  where
    -- node with lowest score+heuristic in openSet
    (current:nexts) = sortOn (aStarGuessScore asMap defVal) openSet
    -- neighbors to consider
    neighbors = getNeighbors current
    pathScoreNext = aStarPathScore asMap defVal current

    (newAsMap, newNext) = foldr opAdd (asMap, nexts) neighbors
      where
        -- opAdd :: (Float, Node) -> (AstarMap Node, [Node]) -> (AstarMem Node, [Node])
        opAdd (dist, node) (mem, ns)
          | pathScoreNext + dist < aStarPathScore mem defVal node =
            (Map.insert node (ASN (pathScoreNext + dist)
                                  (pathScoreNext + dist + hDist node)
                                  (Just current)) mem,
             if notElem node ns then (node:ns) else ns)
          | otherwise = (mem, ns)

-- | aStarPathScore : score from start to node
aStarPathScore :: Ord a => AstarMap a b -> AstarNode a b -> a -> b
aStarPathScore  asMap defVal node = gScore $ Map.findWithDefault defVal node asMap
-- | aStarGuessScore : guessed score from start to end
aStarGuessScore :: Ord a => AstarMap a b -> AstarNode a b -> a -> b
aStarGuessScore asMap defVal node = fScore $ Map.findWithDefault defVal node asMap

-- | Reconstruct a path ending in endNode using the Memory
--   aStarReconstruc memory [endNode]
aStarReconstructPath :: Ord a
  => AstarMap a b -- Memory of the A* search
  -> [a]          -- actual path
  -> [a]          -- reconstructed path
aStarReconstructPath _ [] = []
aStarReconstructPath asMap (n:ns) = case previous $ asMap Map.! n of
  Nothing -> (n:ns)
  Just parent -> aStarReconstructPath asMap (parent:n:ns)
