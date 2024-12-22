{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module TestStateMemo where

import qualified Data.Map as Map
import qualified Control.Monad.State as CMS
import qualified Control.Monad.Identity as CMI
import qualified Control.Monad as CM


main :: IO ()
main = do
  print "======================================================"
  print "== Caching using State Monad                        =="
  print "======================================================"

  print "__fibBasic: no caching"
  print $ "fibBasic 7 = " ++ show (fibBasic 7)

  putStrLn "\n__fibMap: cache using Map"
  print $ "fibMap 7 Map.empty=" ++ show (runFibMap 7)

------- utStrLn "\n__fibState: using State monad"
  print "fibMap :: Int -> Cache -> (Int, Cache) peut aussi être vue comme"
  print "fibMap :: Int -> (Cache -> (Int, Cache))   càd une fonction renvoyant une fonction"
  print "                   or s -> (a, s) sont des State monad, defined in Control.Monad.State"
  print $ "fibState 8 Map.empty=" ++ show (runFibState 8)

  putStrLn ""
  print $ "fibStateP 8 Map.empty=" ++ show (runFibStateP 8)

  putStrLn ""
  print $ "fibStatePB 8 Map.empty=" ++ show (runFibStatePB 8)

  putStrLn ""
  print $ "fibStatePBS 9 Map.empty=" ++ show (runFibStatePBS 9)
  print $ "MAIS cette version efface s a la fin"

  putStrLn ""
  print $ "fibStatePBSRec 9 Map.empty=" ++ show (runFibStatePBSRec 9)

  putStrLn ""
  print $ "A ce point, on pourrait passer au 'do', mais prefere Lift"

  putStrLn "\n__fibLift : lifting computation to the State"
  print $ "fibLift 6 Map.empty=" ++ show (runFibLift 6)
  print $ "en isolant la partie computation"
  print $ "fibLiftIso 6 Map.empty=" ++ show (runFibLift 6)

  putStrLn "\n__fibModule : abstract memoization module"
  print $ "fibLiftMOdule 10 empty=" ++ show (runFibLiftMOdule 10)

  putStrLn "\n__fibApplicative : using applicative style"
  print "as   'CM.liftM2 (+) (fibLiftMOdule (n - 1)) (fibLiftMOdule (n - 2))'"
  print "  => '(+) <$> fib (n-1) <*> fib (n-2'"
  print $ "fibApplicative 8 empty=" ++ show (runFibApplicative 8)

  putStrLn "\n__tidy up : lambda case and MonadState"
  print "LambdaCase LANGUAGE change the case of memo"
  print "  and the type can be made more generic"
  print $ "fibTidy 8 empty=" ++ show (runFibTidy 8)

-- State Monad : tidy up *******************************************************

runFibTidy :: Int -> (Int, Cache)
runFibTidy n = CMS.runState (fibTidy n) Map.empty

-- pure instead of CMS.StateT (\s -> CMI.Identity (0, s))
fibTidy :: Int -> CMS.State Cache Int
fibTidy n = memoTidy n (f n) where
  f 0 = pure 0
  f 1 = pure 1
  f n = (+) <$> fibTidy (n - 1) <*> fibTidy (n - 2)

memoTidy :: (CMS.MonadState (Map.Map k b) m, Ord k) => k -> m b -> m b
memoTidy n compute = CMS.gets (Map.lookup n) >>= \case
  Just v -> pure v
  Nothing ->  compute >>= \res ->
                 CMS.modify (Map.insert n res) >> pure res

-- State Monad : applicative *************************************************

runFibApplicative :: Int -> (Int, Cache)
runFibApplicative n = CMS.runState (fibApplicative n) Map.empty

-- pure instead of CMS.StateT (\s -> CMI.Identity (0, s))
fibApplicative :: Int -> CMS.State Cache Int
fibApplicative n = memo n (f n) where
  f 0 = pure 0
  f 1 = pure 1
  f n = (+) <$> fibApplicative (n - 1) <*> fibApplicative (n - 2)

-- memo :: Int -> CMS.State Cache Int -> CMS.State Cache Int
-- memo n compute = CMS.gets (Map.lookup n) >>= \s -> case s of
--   Just v -> pure v
--   Nothing ->  compute >>= \res ->
--                  CMS.modify (Map.insert n res) >> pure res
-- State Monad : separate memo *************************************************

runFibLiftMOdule :: Int -> (Int, Cache)
runFibLiftMOdule n = CMS.runState (fibLiftMOdule n) Map.empty

-- pure instead of CMS.StateT (\s -> CMI.Identity (0, s))
fibLiftMOdule :: Int -> CMS.StateT Cache CMI.Identity Int
fibLiftMOdule 0 = pure 0
fibLiftMOdule 1 = pure 1
fibLiftMOdule n = memo n compute
  where
    compute = CM.liftM2 (+) (fibLiftMOdule (n - 1)) (fibLiftMOdule (n - 2))

memo :: Int -> CMS.State Cache Int -> CMS.State Cache Int
memo n compute = CMS.gets (Map.lookup n) >>= \s -> case s of
  Just v -> pure v
  Nothing ->  compute >>= \res ->
                 CMS.modify (Map.insert n res) >> pure res
-- State Monad with LiftM2 (2 arguments) ***************************************
-- isolate the computation inside fibLift

runFibLiftIso :: Int -> (Int, Cache)
runFibLiftIso n = CMS.runState (fibLiftIso n) Map.empty

-- pure instead of CMS.StateT (\s -> CMI.Identity (0, s))
fibLiftIso :: Int -> CMS.StateT Cache CMI.Identity Int
fibLiftIso 0 = pure 0
fibLiftIso 1 = pure 1
fibLiftIso n = CMS.gets (Map.lookup n) >>= \s -> case s of
  Just v -> pure v
  Nothing ->  compute >>= \res ->
                 CMS.modify (Map.insert n res) >> pure res
  where
    compute = CM.liftM2 (+) (fibLiftIso (n - 1)) (fibLiftIso (n - 2))

-- State Monad with LiftM2 (2 arguments) ***************************************
-- LiftM2 raise (binary) op to Monad

runFibLift :: Int -> (Int, Cache)
runFibLift n = CMS.runState (fibLift n) Map.empty

-- pure instead of CMS.StateT (\s -> CMI.Identity (0, s))
fibLift :: Int -> CMS.StateT Cache CMI.Identity Int
fibLift 0 = pure 0
fibLift 1 = pure 1
fibLift n = CMS.gets (Map.lookup n) >>= \s -> case s of
  Just v -> pure v
  Nothing -> CM.liftM2 (+) (fibLift (n - 1)) (fibLift (n - 2)) >>= \res ->
                 CMS.modify (Map.insert n res) >> pure res

-- State Monad using >>= than binds (use, get result and feed to next **********

runFibStatePBSRec :: Int -> (Int, Cache)
runFibStatePBSRec n = CMS.runState (fibStatePBSRec n) Map.empty

-- pure instead of CMS.StateT (\s -> CMI.Identity (0, s))
fibStatePBSRec :: Int -> CMS.StateT Cache CMI.Identity Int
fibStatePBSRec 0 = pure 0
fibStatePBSRec 1 = pure 1
fibStatePBSRec n = CMS.get >>= \s -> case Map.lookup n s of
-- s'écrit aussi
--             n = CMS.gets (Map.lookup n) >>= \s -> case s of
  Just v -> pure v
  Nothing -> fibStatePBSRec (n - 1) >>= \r1 ->
               fibStatePBSRec (n - 2) >>= \r2 ->
                 let res = r1 + r2
                 in CMS.get >>= \s2 -> CMS.put (Map.insert n res s2) >>= \_ -> pure res
                 -- qui s'écrit aussi
                 -- in CMS.modify newS >> pure rs

-- State Monad using >>= than binds (use, get result and feed to next **********

runFibStatePBS :: Int -> (Int, Cache)
runFibStatePBS n = CMS.runState (fibStatePBS n) Map.empty

-- pure instead of CMS.StateT (\s -> CMI.Identity (0, s))
fibStatePBS :: Int -> CMS.StateT Cache CMI.Identity Int
fibStatePBS 0 = pure 0
fibStatePBS 1 = pure 1
fibStatePBS n = CMS.get >>= \s -> case Map.lookup n s of
  Just v -> pure v
  Nothing -> fibStatePBS (n - 1) >>= \r1 ->
               fibStatePBS (n - 2) >>= \r2 ->
                 let res = r1 + r2
                     newS = Map.insert n res s
                 in CMS.put newS >>= \_ -> pure res

-- State Monad using >>= than binds (use, get result and feed to next **********

runFibStatePB :: Int -> (Int, Cache)
runFibStatePB n = CMS.runState (fibStatePB n) Map.empty

-- pure instead of CMS.StateT (\s -> CMI.Identity (0, s))
fibStatePB :: Int -> CMS.StateT Cache CMI.Identity Int
fibStatePB 0 = pure 0
fibStatePB 1 = pure 1
fibStatePB n = CMS.StateT $ \s -> case Map.lookup n s of
  Just v -> CMI.Identity (v, s)
  Nothing -> CMS.runStateT (fibStatePB (n - 1)) s >>= \(r1, s1) ->
               CMS.runStateT (fibStatePB (n - 2)) s1 >>= \(r2, s2) ->
                 let res = r1 + r2
                     newS = Map.insert n res s2
                 in CMI.Identity (res, newS)

-- State Monad using pure x = \s -> (x, s) do not change s **********************

runFibStateP :: Int -> (Int, Cache)
runFibStateP n = CMS.runState (fibStateP n) Map.empty

-- pure instead of CMS.StateT (\s -> CMI.Identity (0, s))
fibStateP :: Int -> CMS.StateT Cache CMI.Identity Int
fibStateP 0 = pure 0
fibStateP 1 = pure 1
fibStateP n = CMS.StateT $ \s -> case Map.lookup n s of
  Just v -> CMI.Identity (v, s)
  Nothing -> CMI.Identity (res, newS)
    where
      (r1, s1) = CMS.runState (fibStateP (n - 1)) s
      (r2, s2) = CMS.runState (fibStateP (n - 2)) s1
      res = r1 + r2
      newS = Map.insert n res s2

-- State Monad *****************************************************************

runFibState :: Int -> (Int, Cache)
runFibState n = CMS.runState (fibState n) Map.empty

-- better to put Cache after the arguments (closer to State-like)
fibState :: Int -> CMS.StateT Cache CMI.Identity Int
fibState 0 = CMS.StateT (\s -> CMI.Identity (0, s))
fibState 1 = CMS.StateT (\s -> CMI.Identity (1, s))
fibState n = CMS.StateT $ \s -> case Map.lookup n s of
  Just v -> CMI.Identity (v, s)
  Nothing -> CMI.Identity (res, newS)
    where
      (r1, s1) = CMS.runState (fibState (n - 1)) s
      (r2, s2) = CMS.runState (fibState (n - 2)) s1
      res = r1 + r2
      newS = Map.insert n res s2

-- Cache using Map.Map Int Int *************************************************
type Cache = Map.Map Int Int

runFibMap :: Int -> (Int, Cache)
runFibMap n = fibMap n Map.empty

-- better to put Cache after the arguments (closer to State-like)
fibMap :: Int -> Cache -> (Int, Cache)
fibMap 0 mem = (0, mem)
fibMap 1 mem = (1, mem)
fibMap n mem = case Map.lookup n mem of
  Just v -> (v, mem)
  Nothing -> (res, newMem)
    where
      (r1, mem1) = fibMap (n - 1) mem
      (r2, mem2) = fibMap (n - 2) mem1
      res = r1 + r2
      newMem = Map.insert n res mem2


-- Plain version of fibonacci suite ********************************************
fibBasic :: Int -> Int
fibBasic 0 = 0
fibBasic 1 = 1
fibBasic n = fibBasic (n - 1) + fibBasic (n - 2)
