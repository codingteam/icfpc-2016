
module Solution where

import Data.List
import Data.Ratio
import Data.Maybe

import Problem
import Solver

formatSolution :: [TransformedPolyon] -> String
formatSolution tps =
  let polys = map snd tps
      vertices = allVertices polys
  in  srcPart vertices ++ facetsPart polys vertices ++ dstPart tps vertices

formatNumber :: Number -> String
formatNumber x = go (numerator x) (denominator x)
  where 
    go n 1 = show n
    go n d = show n ++ "/" ++ show d

formatPoint :: Point -> String
formatPoint (x,y) = formatNumber x ++ "," ++ formatNumber y

formatPolygon :: Polygon -> String
formatPolygon poly = unwords $ map formatPoint poly

allVertices :: [Polygon] -> [Point]
allVertices polys = nub $ sort $ concat polys

srcPart :: [Point] -> String
srcPart points =
  unlines (show (length points) : map formatPoint points)

facetsPart :: [Polygon] -> [Point] -> String
facetsPart polys points =
    unlines (show (length polys) : map formatPoly polys)
  where
    formatPoly :: Polygon -> String
    formatPoly poly =
      unwords $ show (length poly) : map showVertex poly
    
    showVertex :: Point -> String
    showVertex p = show (fromJust $ findIndex (==p) points)

-- find index of polygon in list and index of vertex in that polygon
findVertex :: [Polygon] -> Point -> (Int, Int)
findVertex polys p =
  let Just polyIdx = findIndex (\poly -> p `elem` poly) polys
      Just vertexIdx = findIndex (== p) (polys !! polyIdx)
  in  (polyIdx, vertexIdx)

dstPart :: [TransformedPolyon] -> [Point] -> String
dstPart tps points =
    unlines $ map formatPoint $ map go points
  where
    polys = map snd tps
    go vertex =
      let (polyIdx, vertexIdx) = findVertex polys vertex
          transformedPoly = applyTransform (tps !! polyIdx)
      in  transformedPoly !! vertexIdx

