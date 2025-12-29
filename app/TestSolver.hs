
module TestSolver where

{- testing and demo of the MySolver module.

- gaussElimination, reduction of Gauss Eliminated Matrix
-}

import qualified MySolver as MS
import System.Environment (getArgs)

main :: IO ()
main = do
  print "==============================================="
  print "== Test of Solver methods                    =="
  print "==============================================="

  -- MS.VMat on the input line
  args <- getArgs
  if length args >= 1
    then do
        let mat = MS.mkMat (read (head args))
        testGE mat
    else MS.test01
  print "END ==========================================="

-- apply GaussElimination and (if possible) reduction
testGE :: MS.VMat -> IO ()
testGE mat = do
  putStrLn $ "mat *****\n" ++ MS.niceSystem mat
  putStrLn $ MS.tabMat mat

  let res = MS.gaussElimM mat 0
  print "** Gauss Elimination ******************"
  putStrLn $ either show MS.tabMat res
  print "** Reduction **************************"
  case res of
    Left _    -> print "No reduction possible"
    Right geMat -> do
      let red = MS.reduceMatrix geMat
      putStrLn $ MS.tabMat red

