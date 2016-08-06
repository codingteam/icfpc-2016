
module Main where

import Data.Ratio

import Printer
import Problem
import Solver

fieldSize :: Number
fieldSize = 1 / 1

squarePolygon :: Polygon
squarePolygon = [(0, 0), (fieldSize, 0), (fieldSize, fieldSize), (0, fieldSize)]

squareSkeleton :: Skeleton
squareSkeleton =
  [
    ((0, 0), (fieldSize, 0)),
    ((fieldSize, 0), (fieldSize, fieldSize)),
    ((fieldSize, fieldSize), (0, fieldSize)),
    ((0, fieldSize), (0, 0))
  ]

initialProblem :: Problem
initialProblem = Problem {
  pSilhouette = [squarePolygon],
  pSkeleton = squareSkeleton
}

initialSegment :: Segment
initialSegment = ((0, 0), ((1 % 2), (1 % 2)))

foldProblem :: Segment -> Problem -> Problem
foldProblem seg problem =
  let silhouette = pSilhouette problem
      skeleton = pSkeleton problem
  in Problem {
    pSilhouette = silhouette,
    pSkeleton = skeleton
  }

main :: IO ()
main = putStrLn $ serializeProblem $ foldProblem initialSegment initialProblem
