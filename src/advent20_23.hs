{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified MyUtils as MU
-- ****** Data.String.Utils ** split, join, startswith, replace, endsWith
-- import qualified Data.Map.Strict as Map
-- ****** Data.Char: digitToInt, isDigit, isHexDigit, toLower
import Data.Char (digitToInt)
-- import Text.Read (readMaybe)
-- import qualified Data.Massiv.Array as A
-- import Control.Monad ( fold )
-- ****** Data.Maybe: fromJust, fromMaybe, catMaybes, isNothing, listToMaybe, mapMaybe
-- import Data.Maybe ( catMaybes, fromMaybe )
-- ****** Data.IntSet: fromList, toList, split
-- import qualified Dat.IntSet as IS
-- import qualified Data.Map as Map
-- import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Foldable (toList) -- can be used on Seq

import qualified Data.Vector as VU
import qualified Data.Vector.Mutable as VUM
import Control.Monad.ST (runST)
-- import qualified Linear.V2 as LV
-- ****** Data.List: find, sortOn, groupBy, sort, group, delete, (\\), foldl'
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

dataTest = "389125467" :: String
sTest = Seq.fromList (map digitToInt dataTest) :: Cups
dataReal = "583976241" :: String
sReal = Seq.fromList (map digitToInt dataReal) :: Cups

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2020 - Day 23 Part - & -                                          **"
  putStrLn "********************************************************************************"
  -- content <- readFile "Input20/inputxx.txt"
  -- content <- readFile "Input20/testxx_1.txt"

  let start = Seq.fromList (map digitToInt dataReal) :: Cups
  putStrLn $ "start=" ++ show start
  let final = MU.applyN move 100 start
  putStrLn $ "final=" ++ show final
  let pRes = score final
  putStrLn $ "Answer 1> " ++ show pRes


  -- TODO => not working, too much copy of immutable structures....
  -- let startLarge = Seq.fromList (map digitToInt dataReal ++ [10 .. 14]) :: Cups
  -- let (mem, finalLarge) = MU.applyN moveMem 25 ([], startLarge)
  -- -- let foundCups = findCups finalLarge
  -- putStrLn $ "foundCups=" ++ show finalLarge
  -- putStrLn $ "mem=" ++ show mem
  -- putStrLn $ show (toList startLarge)
  -- putStrLn $ "-----\n" ++ concatMap (\s -> show (toList s) ++ "\n") (reverse mem)
  -- lets try bruteforce

  let cOrder = solm sReal 1000000 10000000 9
  let cRes = (cOrder !! 0) * (cOrder !! 1)
  putStrLn $ "Answer 2> " ++ show cRes

  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************
type Cups = Seq.Seq Int

move :: Cups -> Cups
move cups = (before Seq.>< taken) Seq.>< after
  where
    (cursor Seq.:<| body) = cups  -- always works if non-empty
    (taken, leftOver) = Seq.splitAt 3 body
    tmpSeq = leftOver Seq.|> cursor
    (before, after) = Seq.splitAt (1 + nextCursor cursor tmpSeq) tmpSeq

nextCursor :: Int -> Cups -> Int
nextCursor cursor cups = case Seq.elemIndexL decCursor cups of
  Nothing -> nextCursor decCursor cups
  Just idx -> idx
  where
    decCursor = if cursor == 1 then 9 else cursor - 1

score :: Cups -> String
score cups = concatMap show (toList seqFinal)
  where
    Just idx = Seq.elemIndexL 1 cups
    (before, after) = Seq.splitAt idx cups
    seqFinal = Seq.drop 1 after Seq.>< before
-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************

{- Will us a Mutable Unboxed Vector (Unboxed as it will hold a primitive type).
   A Mutable will run in a Monad, usually the ST Monad ("Simple THread").
   The idea is that the Mutable.Vector will only be available IN the Monad,
   and not from any other instance of any Monad. => Keep pure.

  At each index 'i', store the value of the cups label to its right.

  Some clarification about Vector.Mutable, types etc.
  The MVector s n
   - s in the kind of "State Context" in which the vector will be stored. It can
     be the 'RealWorld' (low level implementation IO og GHC) when using MVector
     with th `IO` Monad. And "s" when using the "ST s" Monad.
     To ensure this, and also that only `IO` and `ST s` can be used, a PrimMonas
     class has been defined like this, with two instance:
```heskell
`-- | Class of primitive state-transformer monads
class Monad m => PrimMonad m where
  -- | State token type
  type PrimState m

-- 2 instances
instance PrimMonad IO where
  type PrimState IO = RealWorld

instance PrimMonad (ST s) where
  type PrimState (ST s) = s
``

  - n is the type contained in the vector (I guess).

So, usually, the compiler says the type of a MVector is
```haskell
Vector.Mutable.PrimMonad m =>
  Vector.Mutable.MVector (Vector.Mutable.PrimState m) Int     -- if vector of Int
```

I've seen also:
```haskell
forall m s. (PrimMonad m, PrimState m ~ s) =>
  MVector s Int
```

The "|" is for "type equality", thus inforcing that `s` is the PrimState of the
associated Monad used in the Vector.Mutable.
-}

type CupState s = VUM.MVector s Int

-- This function will be needed to initialize the Vector that will then
-- be used in `` to initialize the Mutable Vector.
-- for an index 'i', find the value of the element of 'cups' at the Right
-- of elem of val 'i+1'
generateIdx
  :: Cups -- base cups ordered (input of the problem)
  -> Int  -- maximum cup label (after the base ones)
  -> Int  -- index of the [Mutable.]Vector
  -> Int  -- RETURNS: the "value" to put at that index (i.e. value of right cup)
generateIdx cups sizeCircle i
  -- right of en item of cups
  | toVal i <= Seq.length cups = valRight
  -- right of last item => first
  | toVal i == sizeCircle = Seq.index cups 0
  -- right of an item of [10..]
  | otherwise = 1 + toVal i
  where
    valRight = case Seq.elemIndexL (toVal i) cups of
      Nothing -> error "in fact, as (toVal i) should be found"
      Just posVal -> if posVal < (Seq.length cups - 1)
                        then Seq.index cups (posVal+1)  -- right is still in Seq
                        else if sizeCircle > Seq.length cups then 10 -- take more than Seq
                                                             else Seq.index cups 0 -- first of Seq



-- conversion idx <-> val (in the "ring" of cup as aMutable.Vector)
toVal :: Int -> Int
toVal idx = idx+1
toIdx :: Int -> Int
toIdx val = val-1

-- decrease Value in the ring, circling from 1 to sizeMax
circlingSub :: (Ord a, Num a) => a -> a -> a
circlingSub val sizeMax
  | val > 1 = val-1
  | otherwise = sizeMax

-- extract the 'n' values at the right of the value at index 'pos' in the "cups ring"
extractNumberAfterPos
  :: (Eq p, VUM.PrimMonad m, VUM.PrimState m ~ s, Num p)
  => Int  -- the Pos/Index where to begin taking the right neighbors
  -> p    -- the number of right values to extract
  -> CupState s -- the ring of Cups
  -> m [Int]  -- RETURNS: list of right values (IN the monad)
extractNumberAfterPos pos n cs = opE 0 pos
  where
    opE m i
      | m == n = pure []
      | otherwise = do
          nxt <- VUM.read cs i
          -- putStrLn $ "ENA: read" ++ show nxt ++ " at idx=" ++ show i
          fmap (nxt:) (opE (m+1) (toIdx nxt))


-- make ONE move on the "cups ring"
-- (ring, current pos) -> new pos (and mofified ring, in the Monad)
moveState
  :: (VUM.PrimMonad m, VUM.PrimState m ~ s)
  => CupState s   -- the "ring" of cups
  -> Int          -- the current Pos/Idx in the ring
  -> Int          -- maximum value (and size) of the ring
  -> m (Int)      -- RETURNS: nex Pos/Idx in the ring
moveState cupsState pos valMax = do
  -- get the next cups labels
  ((c1, c2, c3), newVal) <- pullCups pos
  -- update right of previous pos
  VUM.write cupsState pos newVal
  -- find a valid place for the pulled cups
  let validVal = validCursor (toVal pos) [c1, c2, c3]
  -- and put the taken cups there
  afterVal3 <- VUM.read cupsState (toIdx validVal)
  VUM.write cupsState (toIdx c3) afterVal3
  VUM.write cupsState (toIdx validVal) c1
  pure (toIdx newVal)
  where
    -- pullCups :: Int -> m ((Int, Int, Int), Int)
    pullCups idx = do
      v0 <- VUM.read cupsState idx
      v1 <- VUM.read cupsState (toIdx v0)
      v2 <- VUM.read cupsState (toIdx v1)
      v3 <- VUM.read cupsState (toIdx v2)
      pure ((v0, v1, v2), v3)

    -- validCursor :: Int -> [Int] -> Int
    validCursor val taken
      | elem decVal taken = validCursor decVal taken
      | otherwise = decVal
      where decVal = circlingSub val valMax


-- sol :: Seq.Seq Int -- base sequence of cups labels
--   -> Int         -- nb of cups in the circle
--   -> Int           -- nb of cups label to read after '1'
--   -> [Int]       -- RETURN: list of numbers after '1'
sol cups sizeCircle nbRead = runST $ do
  cs <- VUM.new sizeCircle
  mapM_  (\i -> VUM.write cs i (vinit VU.! i)) [0 .. sizeCircle-1]
  let (cursorVal Seq.:<| body) = cups  -- always works if non-empty
  let pos = toIdx cursorVal
  oldCups <- extractNumberAfterPos pos nbRead cs
  -- newPos <- moveState cs (toIdx cursorVal) sizeCircle
  ((val1, val2, val3), newVal) <- pullCups cs pos
  -- find a valid place for the pulled cups
  let validVal = validCursor (toVal pos) [val1, val2, val3]

  -- -- update right of previous pos
  -- VUM.write cs pos newVal
  -- -- and put the taken cups there
  -- afterVal3 <- VUM.read cs (toIdx validVal)
  -- VUM.write cs (toIdx val3) afterVal3
  -- VUM.write cs (toIdx validVal) val1

  newCups <- extractNumberAfterPos (toIdx validVal) nbRead cs
  -- pure (cursorVal, toIdx cursorVal)
  pure ((cursorVal, pos), oldCups, ((val1, val2, val3), newVal), validVal, newCups)


  where
    vinit = VU.generate sizeCircle (generateIdx cups sizeCircle)
    pullCups mVec idx = do
      v0 <- VUM.read mVec idx
      v1 <- VUM.read mVec (toIdx v0)
      v2 <- VUM.read mVec (toIdx v1)
      v3 <- VUM.read mVec (toIdx v2)
      pure ((v0, v1, v2), v3)
    validCursor :: Int -> [Int] -> Int
    validCursor val taken
      | elem decVal taken = validCursor decVal taken
      | otherwise = decVal
      where decVal = circlingSub val sizeCircle

-- runNMoves
--   :: Monad m
--   => (Int -> m Int)  -- function to repeat, with Pos input
--   -> Int             -- initial Pos
--   -> Int             -- nb times to repeat
--   -> m ()            -- RETURNS: nothing in Monad
runNMoves cs pos nbRun valMax = opRun 0 pos
  where
    opRun i pos
      | i == nbRun = pure ()
      | otherwise = do
          newPos <- moveState cs pos valMax
          opRun (i+1) newPos

-- take sizeCircle steps of moveState before returning the list of values
-- right of cups #1
solm :: Seq.Seq Int -- base sequence of cups labels
  -> Int         -- nb of cups in the circle
  -> Int         -- nb of moves
  -> Int         -- nb of cups label to read after '1'
  -> [Int]       -- RETURN: list of numbers after '1'
solm cups sizeCircle nbMoves nbRead = runST $ do
  cs <- VUM.new sizeCircle
  mapM_  (\i -> VUM.write cs i (vinit VU.! i)) [0 .. sizeCircle-1]
  let (cursorVal Seq.:<| _) = cups  -- always works if non-empty

  runNMoves cs (toIdx cursorVal) nbMoves sizeCircle

  -- newCursorVal <- moveState cs (toIdx cursorVal) sizeCircle
  -- ((val1, val2, val3), newVal) <- pullCups cs (toIdx cursorVal)

  extractNumberAfterPos (toIdx 1) nbRead cs
  where
    vinit = VU.generate sizeCircle (generateIdx cups sizeCircle)
