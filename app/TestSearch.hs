module TestSearch where

{- testing and demo of the MySearch module.

- aStar: search best path using A*
-}

import MySearch ( aStar )

main :: IO ()
main = do
  print "==============================================="
  print "== Test of Search methods                    =="
  print "==============================================="

  let solution = aStar ((==) nEnd) hDistanceNode getNeigborsNode nStart
  case solution of
    Nothing -> print "No solution"
    Just (sol, finalMemory) -> putStrLn $ "bestSol=" ++ show sol

{- A simple graph to test *******************************
   (as seen on https://en.wikipedia.org/wiki/A*_search_algorithm)
-}
-- a Node is made of name, straightLineDistance and list of neighbors with
-- distance
data Node b = N String b [(b, Node b)]
  deriving (Eq, Ord)
instance Show (Node b)
  where
    show (N name _ _) = name
nEnd =   N "end" 0 []
nC =     N "C" 4 [(4, nEnd)]
nB =     N "B" 2 [(3, nC)]
nA =     N "A" 4 [(2, nB)]
nE =     N "E" 2 [(2, nEnd)]
nD =     N "D" 4.5 [(3, nE)]
nStart = N "start" 5 [(1.5, nA), (2, nD)]

getNeigborsNode (N _ _ ns) = ns
hDistanceNode (N _ h _) = h

