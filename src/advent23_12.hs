module Main where

-- import qualified MyParser as MP
-- import Data.List ( group, sort, sortOn, sortBy )
import Data.String.Utils ( replace, split )
-- import qualified Data.Map.Strict as Map
-- import Data.Char ( digitToInt, isDigit )
-- import qualified Data.Massiv.Array as A
-- import Data.Maybe ( fromJust )
import Control.Monad ( foldM )
import qualified Data.Map as Map
import Data.List ( sortOn )

l1 = "???.### 1,1,3"
p1 = parseLine l1
m1 = Map.fromList (zip [0..] (pattern p1)) :: Map.Map Int Char
minit = Map.singleton (0, 0) 1 :: CountMap
l2 = "#?#???????#?.? 3,1,2,2"

l3 = ".??..??...?##. 1,1,3"
p3 = parseLine l3
m3 = Map.fromList (zip [0..] (pattern p3)) :: Map.Map Int Char

main :: IO ()
main = do
  putStrLn "********************************************************************************"
  putStrLn "** Advent 2023 - Day 12 Part 1 & 2                                          **"
  putStrLn "********************************************************************************"
  content <- readFile "Input23/input12.txt"
  -- content <- readFile "Input23/test12_1.txt"

  -- let allPatterns = map parseLine (lines content)

  let opStep curSum line = do
        let pat = parseLine line
        print "____"
        print pat

        let nbParsed = arrange (makeRowCursor pat)
        print $ "nbParsed=" ++ show nbParsed
        return (curSum + nbParsed)

  pRes <- foldM opStep 0 (lines content)
  putStrLn $ "Answer 1> " ++ show pRes

  let opStepUnfold curSum line = do
        let pat = parseLineUnfold line
        print "____"
        print pat

        let nbParsed = arrange (makeRowCursor pat)
        print $ "nbParsed=" ++ show nbParsed
        return (curSum + nbParsed)

  cRes <- foldM opStepUnfold 0 (lines content)
  putStrLn $ "Answer 2> " ++ show cRes

  -- let debugStep pat = do
  --       print "____"
  --       print pat
  --       -- let allRepl = map (trimEnd . trimFront . trim) (generatePatterns [""] (pattern pat))
  --       -- --print allRepl
  --       -- let allencode = map encode allRepl
  --       -- --print allencode
  --       -- let okPatt = filter (isValid (code pat)) allRepl
  --       -- print $ "length okPatt=" ++ show (length okPatt)
  --       let nbOK = checkDebug "" 0 (pattern pat) (code pat)
  --       print $ "nbOk=" ++ show nbOK

  -- mapM_ debugStep allPatterns

  -- let allnbValid = map nbValidReplacement allPatterns
  -- print "__nb valid"
  -- print allnbValid

  -- let pRes = sum allnbValid

  -- let pRes = sum (map (\pat -> check 0 (pattern pat) (code pat)) allPatterns)
  -- putStrLn $ "Answer 1> " ++ show pRes

  -- let allPatternsAdv = map parseLineUnfold (lines content)

  -- let debugStepAdv pat = do
  --       print "____"
  --       print pat
  --       -- let allRepl = map (trimEnd . trimFront . trim) (generatePatterns [""] (pattern pat))
  --       -- print allRepl
  --       -- let allencode = map encode allRepl
  --       -- print allencode
  --       -- let okPatt = filter (isValid (code pat)) allRepl
  --       -- print okPatt
  --       let nbOK = checkDebug "" 0 (pattern pat) (code pat)
  --       print $ "nbOkAdv=" ++ show nbOK

  -- mapM_ debugStepAdv allPatternsAdv

  -- let opStepAdv curSum line = do
  --       print "____"
  --       let pat = parseLine line
  --       print pat
  --       print $ "len=" ++ show (length (pattern pat)) ++ ", cst=" ++ show (codeConstraints (code pat))
  --       print $ "simple nbOk=" ++ show (check 0 (pattern pat) (code pat))
  --       let allRepl = map (trimEnd . trimFront . trim) (generatePatterns [""] (pattern pat))
  --       --print allRepl
  --       -- let allencode = map encode allRepl
  --       --print allencode
  --       let okPatt = filter (isValid (code pat)) allRepl
  --       print $ "okPatt= " ++ show (map reverse okPatt)

  --       let patAdv = parseLineUnfold line
  --       print patAdv
  --       let nbOkAdv = check 0 (pattern patAdv) (code patAdv)
  --       print $ "nbOkAdv=" ++ show nbOkAdv
  --       return (curSum + nbOkAdv)

  let opStepCst curSum line = do
        print "____"
        let pat = parseLine line
        print pat

        let nbOk = grow (pattern pat) (code pat)
        -- print $ "len=" ++ show (length (pattern pat)) ++ ", cst=" ++ show (codeConstraints (code pat)) ++ ", nbBins=" ++ show (nbFreeBins (code pat))
        -- let nbFree = length (pattern pat) - codeConstraints (code pat)
        -- -- print $ "dist=" ++ show (distribute nbFree (nbFreeBins (code pat)))

        --let patCst = genPatCst pat
        --print $ "patCst=" ++ show patCst

        let okCst = filter (meetCst (pattern pat)) (genPatCst pat)
        if length okCst /= nbOk then do
          print $ "nbOk=" ++ show nbOk
          print $ "okCst=" ++ show (length okCst)
          error "Mismatch"
        else
          print ""
        -- return (curSum + length okCst)

        let patCst = parseLineUnfold line
        print patCst
        -- print $ "len=" ++ show (length (pattern patCst)) ++ ", cst=" ++ show (codeConstraints (code patCst)) ++ ", nbBins=" ++ show (nbFreeBins (code patCst))

        let nbOkCst = grow (pattern patCst) (code patCst)
        print $ "nbOkCst=" ++ show nbOkCst
        return (curSum + nbOkCst)


  -- cRes <- foldM opStepCst 0 (lines content)

  -- -- let cRes = sum (map (\pat -> check 0 (pattern pat) (code pat)) allPatternsAdv)
  -- putStrLn $ "Answer 2> " ++ show cRes
  putStrLn "END"

-- *****************************************************************************
-- ********************************************************************** Part 1
-- *****************************************************************************

data Condition = Condition { pattern :: String
                           , code :: [Int] }
  deriving Show

parseLine :: String -> Condition
parseLine line = Condition { pattern = head spaceStep
                           , code = decoded }
  where spaceStep = split " " line
        decoded = map read (split "," (head (tail spaceStep)))

generatePatterns :: [String] -> String -> [String]
generatePatterns previous [] = map reverse previous
generatePatterns previous (p:ps)
  | p == '#' || p == '.' = generatePatterns (map (p :) previous) ps
  | p == '?' = generatePatterns (map ('#' :) previous ++ map ('.' :) previous) ps
  | otherwise = error ("generatePatterns unknown character [" ++ [p] ++ "]")

trim :: String -> String
trim ('.':'.':ps) = trim ('.':ps)
trim (p:ps) = p : trim ps
trim [] = ""

trimFront :: String -> String
trimFront "" = ""
trimFront (p:ps)
  | p == '.' = ps
  | otherwise = p:ps
trimEnd :: String -> String
trimEnd pat = reverse (trimFront (reverse pat))


encode :: String -> [Int]
encode "" = []
encode (p:pattern)
  | p == '.' = map length (split "." pattern)
  | otherwise = map length (split "." (p:pattern))

isValid :: [Int] -> String -> Bool
isValid code pattern = encode pattern == code

nbValidReplacement :: Condition -> Int
nbValidReplacement cond = length ( filter (isValid (code cond))
                                   (map (trimFront . trimEnd . trim) (generatePatterns [""] (pattern cond))) )

-- *****************************************************************************
-- ********************************************************************** Part 2
-- *****************************************************************************
parseLineUnfold :: String -> Condition
parseLineUnfold line = Condition { pattern = extendPat 5 (head spaceStep)
                           , code = extendCod 5 decoded }
  where spaceStep = split " " line
        decoded = map read (split "," (head (tail spaceStep)))

extendPat :: Int -> String -> String
extendPat nb pat = tail $ take (nb * (length pat + 1)) (cycle ('?':pat))

extendCod :: Int -> [Int] -> [Int]
extendCod nb cod = take (nb * length cod) (cycle cod)

check :: Int -> String -> [Int] -> Int
check 0 ('.':ps) code = check 0 ps code
check n ('.':ps) (c:cs)
  | n == c = check 0 ps cs
  | otherwise = 0

check n ('#':ps) (c:cs)
  | n >= c = 0
  | otherwise = check (n+1) ps (c:cs)
check n ('#':ps) [] = 0

check n ('?':ps) (c:cs)
  | n == c = check 0 ps cs
  | n > 0 = check (n+1) ps (c:cs)
  | n == 0 = check 1 ps (c:cs) + check 0 ps (c:cs)
check n ('?':ps) []
  | n == 0 = check 0 ps []
  | n > 0 = 0

-- check 0 ('.':ps) [] = check 0 ps [] -- already dealt with at check 0 (".";ps) code
check n "" [c]
  | n == c = 1
  | otherwise = 0
check _ [] (c:cs) = 0 -- fail
check 0 "" [] = 1
check n "" [] = 0
check n p c = error ("n=" ++ show n ++ ", p=" ++ show p ++ ", c=" ++ show c)

-- *****************************************************************************
checkDebug :: String -> Int -> String -> [Int] -> Int
checkDebug prev 0 ('.':ps) code = checkDebug ('.':prev) 0 ps code
checkDebug prev n ('.':ps) (c:cs)
  | n == c = checkDebug ('.':prev) 0 ps cs
  | otherwise = 0

checkDebug prev n ('#':ps) (c:cs)
  | n >= c = 0
  | otherwise = checkDebug ('#':prev) (n+1) ps (c:cs)
checkDebug prev n ('#':ps) [] = 0

checkDebug prev n ('?':ps) (c:cs)
  | n == c = checkDebug ('.':'=':'?':prev) 0 ps cs
  | n > 0 = checkDebug ('#':'=':'?':prev) (n+1) ps (c:cs)
  | n == 0 = checkDebug ('#':'=':'?':prev) 1 ps (c:cs) + checkDebug ('.':'=':'?':prev) 0 ps (c:cs)
checkDebug prev n ('?':ps) []
  | n == 0 = checkDebug ('.':'=':'?':prev) 0 ps []
  | n > 0 = 0

-- checkDebug prev 0 ('.':ps) [] = checkDebug prev 0 ps [] -- already dealt with at checkDebug prev 0 (".";ps) code
checkDebug prev n "" [c]
  | n == c = 1
  | otherwise = 0
checkDebug prev _ [] (c:cs) = 0 -- fail
checkDebug prev 0 "" [] = 1
checkDebug prev n "" [] = 0
checkDebug prev n p c = error (reverse prev ++ "-> n=" ++ show n ++ ", p=" ++ show p ++ ", c=" ++ show c)

-- *****************************************************************************
-- ********************************************************************* New Way
-- *****************************************************************************
codeConstraints :: [Int] -> Int
codeConstraints code = sum code + length code -1

-- use constraints
makeTemplate :: [Int] -> [String]
makeTemplate code = map (\c -> replicate c '#') code

nbFreeBins :: [a] -> Int
nbFreeBins strCst = length strCst + 1

-- distribute the '.' in the possible intervals
distribute :: Int -> Int -> [[Int]]
distribute nbElem 1 = [[nbElem]]
distribute nbElem nbBins = concat (map (\n -> putN n (distribute (nbElem - n) (nbBins - 1))) [0..nbElem])

putN :: Int -> [[Int]] -> [[Int]]
putN n bins = map (n :) bins

genPatCst pat = map (makeSoluce (code pat)) distSpace
  where --t = makeTemplate (code pat)
      nbFree = length (pattern pat) - codeConstraints (code pat)
      distSpace = distribute nbFree (nbFreeBins (code pat))

makeSoluce :: [Int] -> [Int] -> String
makeSoluce (t:ts) (s:ss) = replicate s '.' ++ makeBlock (t:ts) ss

makeSpace [] (s:ss) = replicate s '.'
makeSpace (t:ts) (s:ss) = replicate (s+1) '.' ++ makeBlock (t:ts) ss
makeBlock (t:ts) [0] = replicate t '#'
makeBlock (t:ts) (s:ss) = replicate t '#' ++ makeSpace ts (s:ss)

meetCst "" "" = True
meetCst (p:ps) (s:ss) = (p == '?' || p == s) && meetCst ps ss
meetCst p s = error ("??? meet s=" ++ s ++ "=, p=" ++ p ++ "=")

findCstPat pat = filter (meetCst (pattern pat)) (genPatCst pat)

-- *****************************************************************************
-- **************************************************************** better state
-- *****************************************************************************

-- State = pattern_left + code_left

grow :: String -> [Int] -> Int
grow ['.'] (c:code) = 0
grow ('.':ps) (c:code)
  | codeConstraints (c:code) > length ('.':ps) = 0
  | otherwise = grow ps (c:code)
grow ('.':ps) [] = grow ps []
grow [] [] = 1

grow ['#'] [1] = 1
grow ['#'] (c:code) = 0
grow ('#':ps) (c:code)
  | not okLen = 0
  | ok = grow nextPat code
  | otherwise = 0
    where (ok, nextPat) = putLava ('#':ps) c
          okLen = codeConstraints (c:code) <= length ('#':ps)
grow ('#':ps) [] = 0

grow ['?'] (c:code)
  | c == 1 = grow [] code
  | otherwise = 0
grow ('?':ps) (c:code)
  | not okLen = 0
  | ok = grow nextPat code + grow ps (c:code)
  | otherwise = grow ps (c:code)
    where (ok, nextPat) = putLava ('#':ps) c
          okLen = codeConstraints (c:code) <= length ('?':ps)
grow ('?':ps) [] = grow ps []

grow "" (c:cs) = 0

grow p c = error ("grow p=" ++ show p ++", c=" ++ show c)

-- given State, can we add a #XXX symbol ?
putLava :: String -> Int -> (Bool, String)
putLava ('#':'.':ps) 1 = (True, ps)
putLava ('#':'?':ps) 1 = (True, ps)
putLava ['#'] 1 = (True, [])
putLava ('?':'.':ps) 1 = (True, ps)
putLava ('?':'?':ps) 1 = (True, ps)
putLava ['?'] 1 = (True, [])
putLava [] 1 = (False, [])

putLava ('#':ps) n = putLava ps (n-1)
putLava ('?':ps) n = putLava ps (n-1)
putLava [] n = (False, [])

putLava ('.':ps) _ = (False, [])


-- **********************************************************'*******************
grawVerb :: String -> String -> [Int] -> [(Int, String, String, [Int])]
grawVerb prev ['.'] (c:code) = [(0, reverse prev, ".", c:code)]
grawVerb prev ('.':ps) (c:code)
  | codeConstraints (c:code) <= length ('.':ps) = grawVerb ('.':prev) ps (c:code)
  | otherwise = [(-1, reverse prev, '.':ps, c:code)]
grawVerb prev ('.':ps) [] = grawVerb ('.':prev) ps []
grawVerb prev [] [] = [(1, reverse prev, "", [])]

grawVerb prev ['#'] [1] = [(1, reverse ('#':prev), "", [])]
grawVerb prev ['#'] (c:code) = [(0, reverse prev, "#", c:code)]
grawVerb prev ('#':ps) (c:code)
  | not okLen = [(-1, reverse prev, '#':ps, c:code)]
  | ok = grawVerb ('#':(show c ++ prev)) nextPat code
  | otherwise = [(0, reverse prev, '#':ps, c:code)]
    where (ok, nextPat) = putLava ('#':ps) c
          okLen = codeConstraints (c:code) <= length ('#':ps)
grawVerb prev ('#':ps) [] = [(0, reverse prev, '#':ps, [])]

grawVerb prev ['?'] (c:code)
  | c == 1 = grawVerb ('.':prev) [] code
  | otherwise = [(0, reverse prev, "?", c:code)]
grawVerb prev ('?':ps) (c:code)
  | not okLen = [(-1, reverse prev, '?':ps, c:code)]
  | ok = grawVerb ('#':(show c ++ prev)) nextPat code ++ grawVerb ('.':prev) ps (c:code)
  | otherwise = grawVerb ('.':prev) ps (c:code)
    where (ok, nextPat) = putLava ('#':ps) c
          okLen = codeConstraints (c:code) <= length ('?':ps)
grawVerb prev  ('?':ps) [] = grawVerb ('.':prev) ps []

grawVerb prev "" (c:cs) = [(0, reverse prev, "", c:cs)]

grawVerb prev p c = error ("grawVerb p=" ++ show p ++", c=" ++ show c ++ " prev=" ++ reverse prev)

-- *****************************************************************************
-- ****************************************************** Joining parsing states
-- *****************************************************************************

-- The idea is to "join" cursors (pos in pattern, length of code left) that countS
-- the number of Cursor at this position'
-- blog de Sea_Estate6087
-- https://www.reddit.com/r/adventofcode/comments/18h4ign/2023_day_12_part_2_how_to_approach_part_2/
-- https://github.com/jimflood/aoc2023/blob/main/src/Day12.hs

-- When parsing, these information are available
data RowCursor = RC { mPat :: Map.Map Int Char -- Char at index Int in Pattern
                    , codeLeft :: [Int]
                    , minSize :: Int
                    , lenPat :: Int
                    , posPat :: Int
                    , countKeys :: [CountKey] -- keys in Map keys -> count
                    } deriving Show

makeRowCursor :: Condition -> RowCursor
makeRowCursor pat = RC patMap (code pat) (codeConstraints (code pat)) (length (pattern pat)) 0 [(0,0)]
  where patMap = Map.fromList (zip [0..] (pattern pat))

-- Counting Map (pos in pattern, length code left)
type CountKey = (Int, Int)
type CountMap = Map.Map CountKey Int

-- The different states of parsing:
-- stage 1: */asterix => parse 0 or more '.'
-- stage 2: #/diese   => parse '#' according to code. Here, "cursores are joined.GRHS
-- stage 3: ./spacer  => parse 1 '.'
-- stage 4: $/toEnd  => parse O or more '.' to the end
-- stage 5: + => victory path
--
-- usually loop (1 -> 2 -> 3) then 4 when no more code

-- Any cursor that makes it to the end has accumulated the counts of all cursors that joined it.
total :: CountMap -> [RowCursor] -> Int
total cm cursors = sum $ map total' cursors
    where
        total' rc = cm Map.! head (countKeys rc)
        -- total' _ = error "Cannot occur"

-- state machine that works like a regular expression
arrange :: RowCursor -> Int
arrange x = arrange' [] (Map.singleton (0, 0) 1, [('*', x)])
    where
        arrange' :: [RowCursor] -> (CountMap, [(Char, RowCursor)]) -> Int
        arrange' acc (cm, []) = total cm acc
        arrange' acc (cm, ('*', rc) : rcs) = arrange' acc (prioritize (asterix (cm, rcs) rc))
        arrange' acc (cm, ('#', rc) : rcs) = arrange' acc (prioritize (diese (cm, rcs) rc))
        arrange' acc (cm, ('.', rc) : rcs) = arrange' acc (prioritize (spacer (cm, rcs) rc))
        arrange' acc (cm, ('$', rc) : rcs) = arrange' acc (prioritize (dollar (cm, rcs) rc))
        arrange' acc (cm, ('+', rc) : rcs) = arrange' (rc : acc) (cm, rcs)
        arrange' a b = error ("strange: " ++ show a ++ " " ++ show b)

arrDebug :: RowCursor -> Int -> IO Int
arrDebug x nbSteps = arrDebug' nbSteps [] (Map.singleton (0, 0) 1, [('*', x)])
    where
        arrDebug' :: Int -> [RowCursor] -> (CountMap, [(Char, RowCursor)]) -> IO Int
        arrDebug' 0 _ _ = do
          print "____ 0"
          print $ "ENDED"
          return (-1)
        arrDebug' nbSteps acc (cm, []) = do
          print $ "____ " ++ show nbSteps
          print $ "TOTAL: cm=" ++ show cm
          return (total cm acc)
        arrDebug' nbSteps acc (cm, ('*', rc) : rcs) = do
          print $ "____ ASTERIX " ++ show nbSteps
          niceState (cm, ('*', rc) : rcs)
          arrDebug' (nbSteps - 1) acc (prioritize (asterix (cm, rcs) rc))
        arrDebug' nbSteps acc (cm, ('#', rc) : rcs) = do
          print $ "____ DIESE " ++ show nbSteps
          niceState (cm, ('#', rc) : rcs)
          arrDebug' (nbSteps - 1) acc (prioritize (diese (cm, rcs) rc))
        arrDebug' nbSteps acc (cm, ('.', rc) : rcs) = do
          print $ "____ SPACER " ++ show nbSteps
          niceState (cm, ('.', rc) : rcs)
          arrDebug' (nbSteps - 1) acc (prioritize (spacer (cm, rcs) rc))
        arrDebug' nbSteps acc (cm, ('$', rc) : rcs) = do
          print $ "____ DOLLAR " ++ show nbSteps
          niceState (cm, ('$', rc) : rcs)
          arrDebug' (nbSteps - 1) acc (prioritize (dollar (cm, rcs) rc))
        arrDebug' nbSteps acc (cm, ('+', rc) : rcs) = do
          print $ "____ PLUS " ++ show nbSteps
          niceState (cm, ('+', rc) : rcs)
          arrDebug' (nbSteps - 1) (rc : acc) (cm, rcs)
        arrDebug' nbSteps a b = error ("strange: " ++ show a ++ " " ++ show b)

prioritize :: (CountMap, [(Char, RowCursor)]) -> (CountMap, [(Char, RowCursor)])
prioritize (cm, cursors) = (cm, sortOn (\(_,rc) -> posPat rc) cursors)

-- adds ('#', RCursor in stage 2 beginning at pos + i, i in [O..d])
asterix :: (CountMap, [(Char, RowCursor)]) -> RowCursor -> (CountMap, [(Char, RowCursor)])
asterix (cm, cursors) rc = (cm, foldl opAsterix cursors  [0..(limit 0)])
  where
    opAsterix :: [(Char, RowCursor)] -> Int -> [(Char, RowCursor)]
    opAsterix rcs i = ('#', rc {posPat = posPat rc + i}) : rcs
    -- check how much '.' can be tried
    limit d
      -- room for leftover '#'
      | d == lenPat rc - posPat rc - codeConstraints (codeLeft rc) = d
      -- not beyond end of pattern
      | posPat rc + d >= lenPat rc  = d
      -- forced '#'
      | mPat rc Map.! (posPat rc + d) == '#' = d
      | otherwise = limit (d + 1)

-- match tail (codeLeft rc) times '#'
-- the key (length (codeLeft rc), posPat rc) is where parse path are joined
-- adding their counts
diese :: (CountMap, [(Char, RowCursor)]) -> RowCursor -> (CountMap, [(Char, RowCursor)])
diese (cm, cursors) rc
  -- possible to add ?
  | nodots = diese' (Map.insertLookupWithKey (\ _ newv oldv -> oldv + newv) (length (codeLeft rc), posPat rc) (cm Map.! head (countKeys rc)) cm)
  -- forget about current RC
  | otherwise = (cm, cursors)
  where
    nodots = all (\k -> (k `Map.member` mPat rc) && ((mPat rc Map.! k) /= '.'))
                 [(posPat rc) .. (posPat rc + head (codeLeft rc) - 1)]
    diese' (Nothing, newCM) = (newCM, (nextState, rc { codeLeft = tail (codeLeft rc)
                                                     , minSize = max 0 (minSize rc - head (codeLeft rc) - 1)
                                                     , posPat = posPat rc + head (codeLeft rc)
                                                     , countKeys = (length (codeLeft rc), posPat rc) : countKeys rc
                                                     }) : cursors)
    -- already a cursor at this position, no need to add
    diese' (Just _, newCM) = (newCM, cursors)
    nextState = if length (codeLeft rc) == 1 then '$' else '.'

noDotsDeb mPat pos nb = all (\k -> (k `Map.member` mPat) && ((mPat Map.! k) /= '.'))
                 [pos .. pos+nb-1]

-- match exactly 1 '.'
spacer :: (CountMap, [(Char, RowCursor)]) -> RowCursor -> (CountMap, [(Char, RowCursor)])
spacer (cm, cursors) rc
  -- ok, add a new '*' cursor
  | mPat rc Map.! posPat rc /= '#' = (cm, ('*', rc {posPat = posPat rc + 1}) : cursors)
  | otherwise = (cm, cursors)

-- match zero or more '.' up to the end of the string
dollar :: (CountMap, [(Char, RowCursor)]) -> RowCursor -> (CountMap, [(Char, RowCursor)])
dollar (cm, cursors) rc
  -- ok, add a '+' finished cursor
  | noHashes = (cm, ('+', rc) : cursors)
  -- forget current rc
  | otherwise = (cm, cursors)
  where
    noHashes = all (\k -> (k `Map.member` mPat rc) && ((mPat rc Map.! k) /= '#'))
                 [(posPat rc) .. lenPat rc - 1]

niceRC :: (Char, RowCursor) -> IO ()
niceRC (c, rc) = print $ " " ++ show c ++
  ": pos=" ++ show (posPat rc) ++
  " code=" ++ show (codeLeft rc) ++
  " size=" ++ show (minSize rc) ++
  " ck=" ++ show (countKeys rc)

niceState :: (CountMap, [(Char, RowCursor)]) -> IO ()
niceState (cm, rcs) = do
  print $ "pat=" ++ show (mPat (snd (head rcs)))
  print $ "cm=" ++ show cm
  mapM_ niceRC rcs
