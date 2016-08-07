
module Main where

import Control.Monad
import Control.Monad.State
import System.FilePath
import System.Environment

import Problem
import Solver
import Solution

worker :: [FilePath] -> FilePath -> FilePath -> Problem -> IO ()
worker doneTxt dstDir inFile problem = do
    let basename = takeFileName inFile
    if basename `elem` doneTxt
      then putStrLn $ basename ++ ": already done"
      else do 
           let outFile = dstDir </> basename
           let polygon = head (pSilhouette problem)
           let initState = [([], unitSquare)]
           let (ok, foldedPolys) = runState (simpleSolve1 polygon) initState
           if ok
             then do
               putStrLn $ "Solved: " ++ basename
               writeFile outFile $ formatSolution $ foldedPolys
             else putStrLn $ basename ++ ": simple solver failed"

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 3) $ fail "Synopsis: solveAllSimple done.txt problems/ solutions/"
  let [donePath, srcDir, dstDir] = args
  done <- lines `fmap` readFile donePath
  let doneTxt = map (\n -> "problem_" ++ n ++ ".txt") done
  findSimpleProblems srcDir (worker doneTxt dstDir)
