module Main where

import Control.Monad
import Data.Graph.AStar
import Data.List
import Data.Maybe
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (distance, Point)
import System.Environment

import qualified Data.HashSet as S

import Draw
import EdgeUnfoldSolver
import Parser
import Problem
import Transform

main :: IO ()
main = mainWith mkDiagram

mkDiagram :: FilePath -> IO (Diagram B)
mkDiagram path = do
  problem <- parseProblem path
  let solution = aStar
                   getCandidates
                   distance
                   heuristic
                   goal
                   (pSkeleton problem)
  print solution
  return $
    case solution of
      Just (_:s:_) -> (# lc red) $ foldr1 atop $ map drawSegment s
      _ -> fromVertices []

for :: [a] -> (a -> b) -> [b]
for = flip map

-- | Generates all possible next state of a skeleton by unfolding a single fold
-- in all places where it's possible.
getCandidates :: Skeleton -> S.HashSet Skeleton
getCandidates skel =
  let points = nub . concat $ map (\(p1, p2) -> [p1, p2]) skel
      unfolds = findUnfoldCandidates points skel
      facets = map (findFacets skel) unfolds
      facetPairs = zip unfolds (map genPairs facets)
      flipped = concat $
                for facetPairs $ \(divider, pairs) ->
                  concat $
                  for pairs $ \(one, another) ->
                    [ (flipSkeleton divider one, another)
                    , (one, flipSkeleton divider another)
                    ]
      merged = map (uncurry mergeSkeletons) flipped
  in  S.fromList merged

-- | Given all possible facets present in initial skeleton, generate pairs of
-- skeletons that, when combined, will amount to initial skeleton.
genPairs :: [Skeleton] -> [(Skeleton, Skeleton)]
genPairs (x:y:xs) =
  (x, foldl' mergeSkeletons y xs) : genPairs ((mergeSkeletons x y):xs)
genPairs _ = []

-- | This is always applied to two neighboring states, so we can be totally
-- sure the difference between them is one single unfold.
distance :: Skeleton -> Skeleton -> Integer
distance _ _ = 1

heuristic :: Skeleton -> Integer
heuristic skel = 0

goal :: Skeleton -> Bool
goal skel = ((length skel) == 4)

dist :: Point -> Point -> Double
dist (x1, y1) (x2, y2) = sqrt $ fromRational $ (x1 - x2)^2 + (y1 - y2)^2
