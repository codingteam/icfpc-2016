
module Transform where

import Control.Arrow

import Problem

data TranslationData = TranslationData {
    offsetX :: Number
  , offsetY :: Number
  , scale   :: Number
} deriving Show

{- | Move skeleton and silhouette to (0, 0), then scale them to fit the
 - a square defined by (0, 0) and (1, 1). -}
toUnitSquare :: Silhouette -> (TranslationData, Silhouette)
toUnitSquare silhouette = (translationData, silhouette')
  where
  points = concat silhouette

  xs = map fst points
  ys = map snd points

  minX = minimum xs
  minY = minimum ys

  offsetX = negate minX
  offsetY = negate minY

  maxX = maximum xs
  maxY = maximum ys

  scale = max (maxX - minX) (maxY - minY)

  translationData = TranslationData offsetX offsetY scale
  silhouette' = map (map $ (translate offsetX) *** (translate offsetY)) silhouette
  translate delta = (/ scale) . (+ delta)

{- | Invert scaling and moving applied by @toUnitSquare@. -}
fromUnitSquare :: TranslationData -> Silhouette -> Silhouette
fromUnitSquare (TranslationData dx dy scale) silhouette = silhouette'
  where
  silhouette' = map (map $ (translate dx) *** (translate dy)) silhouette
  translate delta = (\x -> x - delta) . (* scale)
