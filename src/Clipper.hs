
module Clipper where

import Data.Ratio

import Problem
import qualified Algebra.Clipper as C

multiplier :: Integer
multiplier = 100000000

polygon2clipper :: Problem.Polygon -> C.Polygon
polygon2clipper points =
  let intPoints = [C.IntPoint (convert x) (convert y) | (x,y) <- points]
      convert x = round $ realToFrac $ (multiplier % 1) * x
  in  C.Polygon intPoints

clipper2polygon :: C.Polygon -> Problem.Polygon
clipper2polygon (C.Polygon intPoints) =
  [(fromIntegral x % multiplier, fromIntegral y % multiplier) | C.IntPoint x y <- intPoints]

normalizePolygon :: C.Polygon -> IO C.Polygon
normalizePolygon poly@(C.Polygon points) = do
  clockwise <- C.polygonIsClockwise poly
  if clockwise
    then return $ C.Polygon (reverse points)
    else return poly

normalizePolygons :: C.Polygons -> IO C.Polygons
normalizePolygons (C.Polygons polys) = do
  ps <- mapM normalizePolygon polys
  return $ C.Polygons ps

unionPolygons :: [Problem.Polygon] -> [Problem.Polygon] -> IO [Problem.Polygon]
unionPolygons a b = do
  ca <- normalizePolygons $ C.Polygons (map polygon2clipper a)
  cb <- normalizePolygons $ C.Polygons (map polygon2clipper b)
  C.Polygons cr <- C.union ca cb
  return $ map clipper2polygon cr

intersectPolygons :: [Problem.Polygon] -> [Problem.Polygon] -> IO [Problem.Polygon]
intersectPolygons a b = do
  ca <- normalizePolygons $ C.Polygons (map polygon2clipper a)
  cb <- normalizePolygons $ C.Polygons (map polygon2clipper b)
  C.Polygons cr <- C.intersection ca cb
  return $ map clipper2polygon cr

unionSilhouette :: Silhouette -> IO [Problem.Polygon]
unionSilhouette ps = unionPolygons ps []

polygonArea :: Problem.Polygon -> IO Double
polygonArea poly = do
  area <- C.polygonArea (polygon2clipper poly)
  return $ area / (fromIntegral multiplier)^2

