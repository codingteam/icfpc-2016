
module Main where

import Control.Monad
import Control.Monad.State
import System.FilePath
import System.FilePath.Glob
import System.Environment
import System.Timeout

import Problem
import Parser
import Solver
import ConvexHull
import Solution
import Clipper

worker :: [FilePath] -> FilePath -> FilePath -> Problem -> IO ()
worker doneTxt dstDir inFile problem = do
    let basename = takeFileName inFile
    if basename `elem` doneTxt
      then putStrLn $ basename ++ ": already done"
      else do 
           let outFile = dstDir </> basename
           if length (pSilhouette problem) == 1
             then do
                 let polygon = head (pSilhouette problem)
                     hull = convexHull polygon
                 let initState = [([], unitSquare)]
                 let (ok, foldedPolys) = runState (simpleSolve1 hull) initState
                 if ok
                   then do
                     putStrLn $ "Solved: " ++ basename
                     writeFile outFile $ formatSolution $ foldedPolys
                   else putStrLn $ basename ++ ": simple solver failed"
              else do
                 unitedPolys <- unionSilhouette (pSilhouette problem)
                 if length unitedPolys == 1
                   then do
                     putStrLn "Not so simple problem, using Clipper"
                     let hull =  convexHull (head unitedPolys)
                     let initState = [([], unitSquare)]
                     let (ok, foldedPolys) = runState (simpleSolve1 hull) initState
                     if ok
                       then do
                         putStrLn $ "Solved by Clipper: " ++ basename
                         writeFile outFile $ formatSolution $ foldedPolys
                       else putStrLn $ basename ++ ": simple solver failed"
                   else putStrLn "Can't even use Clipper properly"
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
    timeout (10 * 1000 * 1000) $
        worker doneTxt dstDir path problem
