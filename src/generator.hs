
module Main where

import Printer
import Problem

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

main :: IO ()
main = let text = serializeProblem initialProblem
       in putStrLn text
