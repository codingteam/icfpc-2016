
module Main where

import Data.Ratio

import Problem
import Transform
import Solution

unitSquare :: Polygon
unitSquare =
  [(0,0), (1,0), (1,1), (0,1)]


main :: IO ()
main = do
  --let line = ((-1,-1), (2,2))
  let line = ((-1,1%2), (2,2%3))
      (p, q) = cutPolygon line unitSquare
  putStrLn $ formatPolygon p
  putStrLn $ formatPolygon q
