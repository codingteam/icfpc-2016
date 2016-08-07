
module Main where

import Control.Monad
import Control.Monad.State
import Data.Ratio
import System.Random

import Folder
import Printer
import Problem
import Solution
import Solver

fieldSize :: Number
fieldSize = 1 / 1

nOfCuts = 2
maxDenom = 2^16

squarePolygon :: Polygon
squarePolygon = [(0, 0), (fieldSize, 0), (fieldSize, fieldSize), (0, fieldSize)]

randomFold :: StdGen -> Solver ()
randomFold rng = do
  let segs = take nOfCuts randomSegments
  let ctr = center squarePolygon
  forM_ segs $ \e -> doAutoFold ctr (elongate e)
  where
    randomSegments = zip randomPoints (tail randomPoints)
    randomPoints = zip randomRatios (tail randomRatios)
    randomRatios = map (\(x, y) -> (min x y) % (max x y)) $ zip randomNumbers (tail randomNumbers)
    randomNumbers = (randomRs (1, maxDenom) rng :: [Integer])

main :: IO ()
main = do
  rng <- newStdGen
  let problem = execState (randomFold rng) [([], squarePolygon)]
  putStrLn $ formatSolution $ problem
