
module Transform where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.Ratio
import Data.Default

import Problem

-- import Debug.Trace
trace _ x = x
traceShowId = id

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

traceResult :: String -> Polygon -> Polygon
traceResult msg x =
  trace (msg ++ ": " ++ formatPolygon x) x

edgeLength2 :: Segment -> Number
edgeLength2 ((x1,y1), (x2,y2)) =
    dx*dx + dy*dy
  where
    dx = x2-x1
    dy = y2-y1

-- Simplify polygon by rounding all numbers
simplify :: Polygon -> Polygon
simplify poly = map go poly
  where
    go (x,y) = (rnd x, rnd y)
    rnd x = x `approxRational` (1 % 1000000000000)

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
flipPolygon seg points =
  traceResult ("Flip <" ++ formatPolygon points ++ "> around <" ++ formatSegment seg ++ ">") $
    map (flipPoint seg) $ points

flipSilhouette :: Segment -> Silhouette -> Silhouette
flipSilhouette seg polys = map (flipPolygon seg) polys

flipSegment :: Segment -> Segment -> Segment
flipSegment seg (p1, p2) = (flipPoint seg p1, flipPoint seg p2)

flipSkeleton :: Segment -> Skeleton -> Skeleton
flipSkeleton seg segments = map (flipSegment seg) segments

translatePoint :: Point -> Point -> Point
translatePoint (vx,vy) (x,y) = (vx+x, vy+y)

translateSegment :: Point -> Segment -> Segment
translateSegment v (a,b) = (translatePoint v a, translatePoint v b)

translatePolygon :: Point -> Polygon -> Polygon
translatePolygon v poly = map (translatePoint v) poly

translateSilhouette :: Point -> Silhouette -> Silhouette
translateSilhouette v sil = map (translatePolygon v) sil

-- Rotation matrix has form of
-- /   dx   -dy  \
-- |  ----  ---- |
-- |   D     D   |
-- |             |
-- |   dy    dx  |
-- |  ----  ---- |
-- \   D     D   /
--
-- where D = sqrt(dx^2 + dy^2).
-- So we can store only D, dx and dy.
type Rotation = (Number, Number, Number)

-- We are going to rotate everything around (0,0)
rotatePoint :: Rotation -> Point -> Point
rotatePoint (delta,dx,dy) (x,y) =
  let x' = (x*dx - y*dy) / delta
      y' = (x*dy + y*dx) / delta
  in  (x', y')

rotatePolygon :: Rotation -> Polygon -> Polygon
rotatePolygon rot poly = map (rotatePoint rot) poly

unrotatePolygon :: Rotation -> Polygon -> Polygon
unrotatePolygon (delta,dx,dy) poly = map (rotatePoint (delta,dx,-dy)) poly

rotateSilhouette :: Rotation -> Silhouette -> Silhouette
rotateSilhouette rot sil = map (rotatePolygon rot) sil

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

x /. y = trace ("Y: " ++ show y) $ x / y

-- | Calculate the point where lines defined by segments cross
getCrossPoint :: Segment -> Segment -> Maybe Point
getCrossPoint s1 s2 =
  if areParallel
    then Nothing
    else Just (x, y)
  where
  Line a1 b1 c1 = toLine s1
  Line a2 b2 c2 = toLine s2

  delta = a1*b2 - a2*b1

  areParallel = 0 == delta

  deltaX = -c1*b2 + c2*b1
  deltaY = -a1*c2 + a2*c1

  x = deltaX / delta
  y = deltaY / delta

type CutterState = (Polygon, Polygon)

-- | Cut a (convex!) polygon in two by a line defined by a segment
cutPolygon :: Segment -> Polygon -> (Polygon, Polygon)
cutPolygon _   [] = ([], [])
cutPolygon line polygon =
      if any (onLine line) edges -- one of polygon edges 
        then if any (\p -> p `relativeTo` line == OnLeft) polygon
             then (polygon, [])
             else ([], polygon)
        else if all (\p -> p `relativeTo` line `elem` [OnLine, OnLeft]) polygon
               then (polygon, [])
               else if all (\p -> p `relativeTo` line `elem` [OnLine, OnRight]) polygon
                    then ([], polygon)
                    else baseCase
  where
    edges = zip polygon (tail polygon) ++ [(last polygon, head polygon)]

    onLine seg (a,b) = a `relativeTo` seg == OnLine &&
                       b `relativeTo` seg == OnLine

    baseCase = execState cutter ([], [])

    cutter :: State CutterState ()
    cutter = forM_ edges cutEdge

    cutEdge :: Segment -> State CutterState ()
    cutEdge edge@(start,end) = do
      let sideStart = start `relativeTo` line
          sideEnd   = end   `relativeTo` line
      case (sideStart, sideEnd) of
        (OnLeft, OnLine) -> do
            toLeftPolygon start 
            toLeftPolygon end 
            toRightPolygon end
        (OnLine, OnLeft) -> do
            toLeftPolygon start 
            toRightPolygon start
            toLeftPolygon end 
        (OnRight, OnLine) -> do
            toRightPolygon start
            toLeftPolygon end 
            toRightPolygon end
        (OnLine, OnRight) -> do
            toRightPolygon start
            toLeftPolygon start 
            toRightPolygon end
        (OnLeft, OnLeft) -> do
            toLeftPolygon start 
            toLeftPolygon end
        (OnRight, OnRight) -> do
            toRightPolygon start
            toRightPolygon end
        (OnLeft, OnRight) -> do
            let Just cross = getCrossPoint edge line
            toLeftPolygon start
            toLeftPolygon cross
            toRightPolygon cross
            toRightPolygon end
        (OnRight, OnLeft) -> do
            let Just cross = getCrossPoint edge line
            toRightPolygon start
            toRightPolygon cross
            toLeftPolygon cross
            toLeftPolygon end

    toLeftPolygon :: Point -> State CutterState ()
    toLeftPolygon p =
      modify $ \(l,r) -> if p `elem` l
                           then (l,r)
                           else (l ++ [p], r)

    toRightPolygon :: Point -> State CutterState ()
    toRightPolygon p =
      modify $ \(l,r) -> if p `elem` r
                           then (l,r)
                           else (l, r ++ [p])

