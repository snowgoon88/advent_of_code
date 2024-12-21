module MyCache where

import qualified Data.Map as Map
{- ===== Exemple d'utilisation ==========================

import qualified MyCache as MC
import qualified Data.Map as Map

main :: IO ()
main = do
  print "==============================================="
  print "== Test de fonction cache                    =="
  print "==============================================="

  let cachedF = MC.cachedFun testFunc
  let opTest (acc,cache) v = (res:acc, newCache) where (res, newCache) = cachedF cache v

  let res = foldl opTest ([], Map.empty) l
  print $ "res=" ++ show res
  -- res=([10,8,4,9,10,8,4],fromList [(1,4),(5,8),(6,9),(7,10)])

l = [1,5,7,6,1,5,7]

testFunc :: Int -> Int
testFunc n = n+3
-}

cachedFun :: Ord k => (k -> a) -> Map.Map k a -> k -> (a, Map.Map k a)
cachedFun func cacheMap input = case Map.lookup input cacheMap of
  Just res -> (res, cacheMap)
  Nothing -> (res, Map.insert input res cacheMap)
    where res = func input
