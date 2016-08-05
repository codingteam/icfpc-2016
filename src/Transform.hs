
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

-- | Flip point relative to segment
flipPoint :: Segment -> Point -> Point
flipPoint ((sx1,sy1), (sx2,sy2)) (x,y) =
  if sx1 == sx2
    then (2*sx1 - x, y)
    else if sy1 == sy2
           then (x, 2*sy1 - y)
           else let k = (sy2-sy1) / (sx2-sx1)
                    k' = -(sx2-sx1) / (sy2-sy1)
                    b = sy1 - k * sx1
                    b' = y - k' * x
                    x0 = - (b - b') / (k - k')
                    y0 = (b'*k - b*k') / (k - k')
                    x' = 2*x0 - x
                    y' = 2*y0 - y
                in  (x', y')

-- | Flip polygon relative to segment
flipPolygon :: Segment -> Polygon -> Polygon
flipPolygon seg points = map (flipPoint seg) $ reverse points

flipSilhouette :: Segment -> Silhouette -> Silhouette 
flipSilhouette seg polys = map (flipPolygon seg) polys

