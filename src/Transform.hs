
module Transform where

import Control.Arrow
import Data.Default

import Problem

data ToUnitSquareConfig = ToUnitSquareConfig {
    doMove  :: Bool
  , doScale :: Bool
} deriving Show

instance Default ToUnitSquareConfig where
  def = ToUnitSquareConfig True True

data TranslationData = TranslationData {
    conf    :: ToUnitSquareConfig
  , offsetX :: Number
  , offsetY :: Number
  , scale   :: Number
} deriving Show

{- | Move skeleton and silhouette to (0, 0), then scale them to fit the
 - a square defined by (0, 0) and (1, 1). -}
toUnitSquare :: ToUnitSquareConfig -> Problem -> (TranslationData, Problem)
toUnitSquare conf problem = (translationData, problem')
  where
  silhouette = pSilhouette problem
  skeleton = pSkeleton problem

  points = concat silhouette

  xs = map fst points
  ys = map snd points

  (minX, minY) = if doMove conf
                   then (minimum xs, minimum ys)
                   else (minimum $ 0:xs, minimum $ 0:ys)

  offsetX = negate minX
  offsetY = negate minY

  maxX = maximum xs
  maxY = maximum ys

  scale = max (maxX - minX) (maxY - minY)

  translationData = TranslationData conf offsetX offsetY scale
  silhouette' = map (map $ translatePoint offsetX offsetY) silhouette
  skeleton' = map ((translatePoint offsetX offsetY) *** (translatePoint offsetX offsetY)) skeleton
  translateNumber delta point =
    let p'  = if doMove conf then point + delta else point
        p'' = if doScale conf then p' / scale else p'
    in  p''
  translatePoint dx dy = (translateNumber dx) *** (translateNumber dy)

  problem' = Problem silhouette' skeleton'

{- | Invert scaling and moving applied by @toUnitSquare@. -}
fromUnitSquare :: TranslationData -> Silhouette -> Silhouette
fromUnitSquare (TranslationData conf dx dy scale) silhouette = silhouette'
  where
  silhouette' = map (map $ (translate dx) *** (translate dy)) silhouette
  translate delta point =
    let p'  = if doScale conf then point * scale else point
        p'' = if doMove conf then p' - delta else p'
    in  p''

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

flipSegment :: Segment -> Segment -> Segment
flipSegment seg (p1, p2) = (flipPoint seg p1, flipPoint seg p2)

flipSkeleton :: Segment -> Skeleton -> Skeleton
flipSkeleton seg segments = map (flipSegment seg) segments

-- | Possible relative positions of line and point
data RelativePos = OnLeft | OnLine | OnRight
  deriving (Show, Eq)

-- | Find out where the point is relative to the line on which lies the segment
relativeTo :: Point -> Segment -> RelativePos
relativeTo (x, y) segment =
  if sign == 0
    then OnLine
    else if sign > 0
      then OnRight
      else OnLeft
  where
  Line a b c = toLine segment
  sign = a*x + b*y + c

data Line = Line {
    a :: Number
  , b :: Number
  , c :: Number
} deriving (Show, Eq)

toLine :: Segment -> Line
toLine ((x1, y1), (x2, y2)) = Line (y1 - y2) (x2 - x1) (x1*y2 - x2*y1)

-- | Calculate the point where lines defined by segments cross
getCrossPoint :: Segment -> Segment -> Maybe Point
getCrossPoint s1 s2 =
  if areParallel
    then Nothing
    else Just (x, y)
  where
  Line a1 b1 c1 = toLine s1
  Line a2 b2 c2 = toLine s2

  areParallel = 0 == a1*b2 - a2*b1

  x = if b1 == 0 && a1 /= 0
        then negate $ c1 / a1
        else if b2 == 0 && a2 /= 0
          then negate $ c2 / a2
          else (b1*c2 - b2*c1) / (b2*a1 - b1*a2)
  y = if a1 == 0 && b1 /= 0
        then negate $ c1 / b1
        else if a2 == 0 && b2 /= 0
          then negate $ c2 / b2
          else negate $ (a2 * x + c2) / b2

-- | Cut a polygon in two by a line defined by a polygon
cutPolygon :: Segment -> Polygon -> (Polygon, Polygon)
cutPolygon _   [] = ([], [])
cutPolygon seg (p:poly) =
  case p `relativeTo` seg of
    OnLeft  -> go OnLeft ([p], []) (poly ++ [p])
    OnRight -> go OnRight ([], [p]) (poly ++ [p])
    OnLine  -> cutPolygon seg (poly ++ [p])
  where
  tidy [] = []
  tidy input@(x:xs) =
    let xs' = reverse xs
    in  if head xs' == x
          then xs'
          else reverse input

  go _ (left, right) [] = (tidy $ reverse left, tidy $ reverse right)
  go OnLeft (left, right) (p : points) =
    case p `relativeTo` seg of
      OnLine -> go OnRight (p:left, p:right) points
      OnLeft -> go OnLeft (p:left, right) points
      -- the previous point was on the other side of the line
      OnRight ->
        let prev = head left
            -- we can be sure there's a crosspoint because we made sure that
            -- the points are on different sides of the line
            (Just crosspoint) = getCrossPoint seg (prev, p)
        in  go OnRight (crosspoint:left, p:crosspoint:right) points
  go OnRight (left, right) (p : points) =
    case p `relativeTo` seg of
      OnLine  -> go OnLeft  (p:left, p:right) points
      OnRight -> go OnRight (left, p:right) points
      -- the previous point was on the other side of the line
      OnLeft  ->
        let prev = head right
            -- we can be sure there's a crosspoint because we made sure that
            -- the points are on different sides of the line
            (Just crosspoint) = getCrossPoint seg (prev, p)
        in  go OnLeft  (p:crosspoint:left, crosspoint:right) points
