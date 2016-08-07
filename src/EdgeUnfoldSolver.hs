module EdgeUnfoldSolver where

import Control.Arrow
import Data.List
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

import Debug.Trace

import Problem

type SegmentAttribution = (Segment, [Polygon])

facetsFromSkeleton :: Skeleton -> [Polygon]
facetsFromSkeleton skeleton = []
  where
    skeletonNodes segments = nub $ concat $ unzip segments

isFreePolygon :: Polygon -> [SegmentAttribution] -> Bool
isFreePolygon poly segmentAttribution = do
  let segments = filter (\(seg, _) -> seg `elem` (toSegments poly)) segmentAttribution
  let shareCount = length $ filter (>1) $ map (\(_, polys) -> length polys) segments
  not $ shareCount > 1

findSharedEdges :: [Polygon] -> [SegmentAttribution]
findSharedEdges polygons =
  map (\s -> (s, filter (containsSegment s) polygons)) $ nub $ concat $ map toSegments polygons
  where
    containsSegment segment polygon = segment `elem` toSegments polygon

toSegments :: Polygon -> [Segment]
toSegments [] = []
toSegments input@(point : _) = recMakeSegments (input ++ [point])
  where
    recMakeSegments [] = []
    recMakeSegments (_ : []) = []
    recMakeSegments (p : pts) = (p, (head pts)) : recMakeSegments pts

findUnfoldCandidates :: [Point] -> [Segment] -> [Segment]
findUnfoldCandidates vertices segments =
  filter (isUnwrapCandidate vertices) segments
  where
  isUnwrapCandidate vertices segment = do
    let distances = map (distanceFromLine segment) vertices
    let signs = map signum $ filter (/= 0) distances
    case signs of
      (x:xs) -> and $ map (== head signs) (tail signs)
      _      -> False
  distanceFromLine segment vertex = do
    let ((x0, y0), (x1, y1)) = segment
    let (x2, y2) = vertex
    (x1 - x0) * (y2 - y0) - (x2 - x0) * (y1 - y0)

-- | Split @skeleton@ in two parts, one of which will be on the left, another
-- on the right of the given @segment@ when we unfold the @skeleton@.
--
-- The parts aren't complete, closed skeletons! You should immediately merge
-- them with @mergeSkeletons@.
findFacets :: Skeleton -> Segment -> [Skeleton]
findFacets skeleton (startPt, endPt) = go graph startPt []
  where
  graph1 = M.fromListWith S.union $ map (\(k, v) -> (k, S.singleton v)) skeleton
  graph2 = M.fromListWith S.union $ map (\(v, k) -> (k, S.singleton v)) skeleton
  graph = M.unionWith S.union graph1 graph2

  go edges start facet
    | start == endPt =
        -- avoid returning the segment we were given
        if length facet > 1 then [reverse facet] else []
    | M.null edges =
        if snd (head facet) == endPt
          then [facet]
          else []
    | otherwise =
        let next = M.lookupDefault S.empty start edges
            edges' = M.delete start edges

            facets = map (\end -> (end, (start, end):facet)) (S.toList next)
        in  concatMap (\(start', facet') -> go edges' start' facet') facets
