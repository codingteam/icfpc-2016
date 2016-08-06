module Folder where

import Control.Monad.State

import Problem
import Solver

foldPolygon :: Segment -> Polygon -> [Polygon]
foldPolygon seg polygon =
  let transformed = execState (doFoldLeft seg) [([], polygon)]
  in  map applyTransform transformed

foldSilhouette :: Segment -> Silhouette -> Silhouette
foldSilhouette seg sil =
  concat $ map (foldPolygon seg) sil

foldProblem :: Segment -> Problem -> Problem
foldProblem seg problem =
  let silhouette = foldSilhouette seg $ pSilhouette problem
      skeleton = pSkeleton problem
  in Problem {
    pSilhouette = silhouette,
    pSkeleton = skeleton
  }
