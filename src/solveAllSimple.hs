
module Main where

import Control.Monad
import Control.Monad.State
import System.FilePath
import System.FilePath.Glob hiding (simplify)
import System.Environment
import System.Timeout

import Problem
import Transform (simplify)
import Parser
import Solver
import ConvexHull
import Solution

worker :: Bool -> [FilePath] -> FilePath -> FilePath -> Problem -> IO ()
worker doSimplify doneTxt dstDir inFile problem = do
    let basename = takeFileName inFile
    if basename `elem` doneTxt
      then putStrLn $ basename ++ ": already done"
      else do 
           let outFile = dstDir </> basename
           case problem of
            Problem [polygon] _ -> do
              if isConvex polygon || doSimplify
                then do
                   let hull = convexHull polygon
                       simplified = simplify hull
                       target = if doSimplify
                                  then simplified
                                  else polygon
                   let initState = [([], unitSquare)]
                   let (ok, foldedPolys) = runState (simpleSolve1 target) initState
                   if ok
                     then do
                       putStrLn $ "Solved: " ++ basename
                       writeFile outFile $ formatSolution $ foldedPolys
                     else putStrLn $ basename ++ ": simple solver failed"
                else putStrLn $ "Problem too complex without simplification"
            _ -> putStrLn "Problem too complex"

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 3) $ fail "Synopsis: solveAllSimple done.txt problems/ solutions/"
  let [donePath, srcDir, dstDir] = args
  done <- lines `fmap` readFile donePath
  let doneTxt = map (\n -> "problem_" ++ n ++ ".txt") done
  paths <- glob (srcDir </> "*.txt")
  forM_ paths $ \path -> do
    problem <- parseProblem path
    putStrLn $ takeFileName path
    timeout (1 * 1000 * 1000) $
        worker False doneTxt dstDir path problem
