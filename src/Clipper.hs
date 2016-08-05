
module Clipper where

import Data.Ratio

import Problem
import qualified Algebra.Clipper as C

multiplier :: Integer
multiplier = 1000000

polygon2clipper :: Problem.Polygon -> C.Polygon
polygon2clipper points =
  let intPoints = [C.IntPoint (convert x) (convert y) | (x,y) <- points]
      convert x = round $ realToFrac x
  in  C.Polygon intPoints

clipper2polygon :: C.Polygon -> Problem.Polygon
clipper2polygon (C.Polygon intPoints) =
  [(fromIntegral x % multiplier, fromIntegral y % multiplier) | C.IntPoint x y <- intPoints]

unionPolygons :: [Problem.Polygon] -> [Problem.Polygon] -> IO [Problem.Polygon]
unionPolygons a b = do
  let ca = C.Polygons (map polygon2clipper a)
      cb = C.Polygons (map polygon2clipper b)
  C.Polygons cr <- C.union ca cb
  return $ map clipper2polygon cr

unionSilhouette :: Silhouette -> IO Polygon
unionSilhouette [] = return []
unionSilhouette [p] = return p
unionSilhouette [p1,p2] = head `fmap` unionPolygons [p1] [p2]
unionSilhouette (p:ps) = do
  rest <- unionSilhouette ps
  head `fmap` unionPolygons [p] [rest]
