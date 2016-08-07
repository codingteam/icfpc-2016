
module Trivial where

import Control.Monad
import System.FilePath

import Problem
import Parser

edgeLength2 :: Segment -> Number
edgeLength2 ((x1,y1), (x2,y2)) =
    dx*dx + dy*dy
  where
    dx = x2-x1
    dy = y2-y1

isUnitSquare :: Polygon -> Bool
isUnitSquare poly =
    length poly == 4 &&
    all (\e -> edgeLength2 e == 1) edges
  where
    edges = zip poly (tail poly) ++ [(last poly, head poly)]

isTrivialProblem :: Problem -> Bool
isTrivialProblem (Problem [poly] _) = isUnitSquare poly
isTrivialProblem _ = False

trivialSolver :: Problem -> String
trivialSolver (Problem [poly] _) =
  unlines ["4", "0,0", "1,0", "1,1", "0,1", "1", "4 0 1 2 3"] ++
  unlines (map formatPoint poly)

runTrivialSolver :: FilePath -> FilePath -> IO ()
runTrivialSolver inFile outFile = do
  problem <- parseProblem inFile
  when (isTrivialProblem problem) $ do
    putStrLn (takeFileName inFile)
    let result = trivialSolver problem
    writeFile outFile result
  
