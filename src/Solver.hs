{-# LANGUAGE NoMonomorphismRestriction #-}
module Solver where

import Control.Monad
import Control.Monad.State
import Data.List
import System.FilePath
import System.FilePath.Glob
import System.IO

import Problem
import Transform
import Parser

-- import Debug.Trace

type TransformedPolyon = ([Fold], Polygon)
type SolverState = [TransformedPolyon]
data Fold =
    Translate Point -- ^ Map (0,0) to that Point
  | Rotate Rotation -- ^ Rotate around (0,0)
  | FoldLeft Segment
  | FoldRight Segment
  deriving (Eq, Show)

type Solver a = State SolverState a

unitSquare :: Polygon
unitSquare =
  [(0,0), (1,0), (1,1), (0,1)]

isConvex :: Polygon -> Bool
isConvex points =
  let vectors = zipWith mkVector points (tail points) ++ [mkVector (last points) (head points)]
      mkVector (x1,y1) (x2,y2) = (x2-x1, y2-y1)
      vectorProduct (x1,y1) (x2,y2) = x1*y2 - x2*y1
      products = zipWith vectorProduct vectors (tail vectors) ++ [vectorProduct (last vectors) (head vectors)]
  in  all (>= 0) products || all (<= 0) products

isSimpleProblem :: Problem -> Bool
isSimpleProblem (Problem [polygon] _) = isConvex polygon
isSimpleProblem _ = False

findSimpleProblems :: FilePath -> (FilePath -> Problem -> IO ()) -> IO ()
findSimpleProblems dir worker = do
  paths <- glob (dir </> "*.txt")
  forM_ paths $ \path -> do
    problem <- parseProblem path
    when (isSimpleProblem problem) $ do
      putStrLn $ takeFileName path
      worker path problem

elongate :: Segment -> Segment
elongate ((x1,y1), (x2,y2)) =
  if x1 == x2
    then ((x1, 0), (x1, 1))
    else if y1 == y2
           then ((0,y1), (1, y1))
           else let k = (y2-y1) / (x2-x1)
                    b = y1 - k*x1
                in if k > 1
                     then let x1' = -b/k
                              x2' = (1-b)/k
                          in  ((x1', 0), (x2', 1))
                     else let y1' = b
                              y2' = k+b
                          in ((0,y1'), (1, y2'))

foldPolygonLeft :: Segment -> TransformedPolyon -> [TransformedPolyon]
foldPolygonLeft seg (ts, p) =
  let (p1,p2) = cutPolygon seg p
  in trace ("Fold Left: Cut <" ++ formatPolygon p ++ "> with <" ++ formatSegment seg ++ ">:\n\t<" ++
            formatPolygon p1 ++ ">\n\t<" ++ formatPolygon p2 ++ ">") $
      if null p2
        then [(FoldLeft seg: ts, flipPolygon seg p1)]
        else if null p1
             then [(ts, p2)]
             else [(FoldLeft seg: ts, flipPolygon seg p1), (ts, p2)]

foldPolygonRight :: Segment -> TransformedPolyon -> [TransformedPolyon]
foldPolygonRight seg (ts, p) =
  let (p1,p2) = cutPolygon seg p
  in trace ("Fold Right: Cut <" ++ formatPolygon p ++ "> with <" ++ formatSegment seg ++ ">:\n\t<" ++
            formatPolygon p1 ++ ">\n\t<" ++ formatPolygon p2 ++ ">") $
      if null p2
        then [(ts, p1)]
        else if null p1
             then [(FoldRight seg: ts, flipPolygon seg p2)]
             else [(ts, p1), (FoldRight seg: ts, flipPolygon seg p2)]

doFoldLeft :: Segment -> Solver ()
doFoldLeft seg = do
  modify $ \polygons -> concatMap (foldPolygonLeft seg) polygons

doFoldRight :: Segment -> Solver ()
doFoldRight seg = do
  modify $ \polygons -> concatMap (foldPolygonRight seg) polygons

doAutoFold :: Point -> Segment -> Solver ()
doAutoFold ctr seg =
  case ctr `relativeTo` seg of
    OnLeft -> trace ("Fold Right around " ++ formatSegment seg) $ doFoldRight seg
    _ -> trace ("Fold Left around " ++ formatSegment seg) $ doFoldLeft seg

doTranslate :: Point -> Solver ()
doTranslate v =
    modify $ \polygons -> map (translatePolygonT v) polygons
  where
    translatePolygonT :: Point -> TransformedPolyon -> TransformedPolyon
    translatePolygonT v (ts, p) =
      (Translate v: ts,
        trace ("Translate <" ++ formatPolygon p ++ "> by <" ++ formatPoint v ++ ">") translatePolygon v p)

doRotate :: Rotation -> Solver ()
doRotate rot =
    modify $ \polygons -> map (rotatePolygonT rot) polygons
  where
    rotatePolygonT :: Rotation -> TransformedPolyon -> TransformedPolyon
    rotatePolygonT rot (ts, p) =
      (Rotate rot: ts,
        traceResult ("Rotate <" ++ formatPolygon p ++ "> by " ++ show rot) $
          rotatePolygon rot p)

-- | Check if polygon fits in unit square
checkFitsXY :: Polygon -> Solver Bool
checkFitsXY poly = do
  let ok = isInUnitSquare poly
  when (not ok) $
    trace "Polygon does not fit in unit square" $ return ()
  return ok

-- | Check if size of polygon by X,Y is <= 1.
checkSizeXY :: Monad m => Polygon -> m Bool
checkSizeXY poly = do
  let xs = map fst poly
      ys = map snd poly
      maxx = maximum xs
      maxy = maximum ys
      minx = minimum xs
      miny = minimum ys
      dx = maxx-minx
      dy = maxy-miny
  let bad = dx > 1 || dy > 1
  when (bad) $
    trace "Polygon size by X or y is > 1, we need to rotate it first" $ return ()
  return $ trace ("Polygon size: " ++ formatPoint (dx,dy)) $ not bad

translateToOrigin :: Polygon -> Solver ()
translateToOrigin target = do
  let minx = minimum (map fst target)
      miny = minimum (map snd target)
  doTranslate (minx, miny)

guessRotation :: Polygon -> Maybe (Point, Rotation)
guessRotation target = do
    let edges = zip target (tail target) ++ [(last target, head target)]
    guess@(origin,rot) <-  msum $ map check edges
    ok <- checkSizeXY $ unrotatePolygon rot target
    if ok
      then return guess
      else Nothing
  where
    check edge@((x1,y1), (x2,y2)) =
      if edgeLength2 edge == 1
        then let dx = x2-x1
                 dy = y2-y1
                 delta = 1
             in traceShowId $  Just ((x1,y1), (delta, dx, dy))
        else trace "not found" Nothing

isEverythingInside :: Polygon -> Solver Bool
isEverythingInside target = do
    tps <- get
    let silhouette = concatMap snd tps
        edges = zip target (tail target) ++ [(last target, head target)]
    return $ all (atOneSide silhouette) edges
  where
    atOneSide silhouette edge =
        all (\p -> p `relativeTo` edge `elem` [OnLine, OnLeft]) silhouette ||
        all (\p -> p `relativeTo` edge `elem` [OnLine, OnRight]) silhouette

removeSinglePoints :: Solver ()
removeSinglePoints = do
    modify (filter good)
  where
    good (_, poly) = length poly >= 3

-- not used currently
unfoldPolygon :: TransformedPolyon -> Polygon
unfoldPolygon (transforms, p) = applyTransform (reverse transforms, p)

applyTransform :: TransformedPolyon -> Polygon
applyTransform (transforms, p) = go transforms p
  where
    go [] p = p
    go (t:ts) p = go ts $ apply t p

    apply (FoldLeft seg) p = flipPolygon seg p
    apply (FoldRight seg) p = flipPolygon seg p
    apply (Translate (vx,vy)) p = translatePolygon (-vx, -vy) p
    apply (Rotate (delta,dx,dy)) p = rotatePolygon (delta,dx,-dy) p

center :: Polygon -> Point
center poly =
    let (sx,sy) = foldr plus (0,0) poly
        n = fromIntegral (length poly)
    in  (sx / n, sy / n)
  where
    plus (x1,y1) (x2,y2) = (x1+x2, y1+y2)

repeatUntil :: Monad m => m Bool -> m () -> m ()
repeatUntil check action = do
  action
  res <- check
  if not res
    then repeatUntil check action
    else return ()

simpleSolve1 :: Polygon -> Solver Bool
simpleSolve1 target = do
  case guessRotation target of
    Nothing -> do
        translateToOrigin target
    Just (origin, rot) -> do
        doRotate $! trace "rotation found" rot
        doTranslate origin
  -- checkFitsXY target
  tps <- get
  --let silhouette = concatMap snd tps
  let edges = zip target (tail target) ++ [(last target, head target)]
      ctr = center target
  repeatUntil (isEverythingInside target) $ do
      forM_ (zip [0..] edges) $ \(i, edge) -> 
        when (even i) $
          doAutoFold ctr (elongate edge)
      forM_ (zip [0..] edges) $ \(i, edge) -> 
        when (odd i) $
          doAutoFold ctr (elongate edge)
  removeSinglePoints
  return True

runSimpleSolver :: Polygon
                -> (Silhouette -> a)
                -> ([TransformedPolyon] -> IO b)
                -> a
                -> IO a
runSimpleSolver polygon withUnfolded withFolded onFail = do
         let initState = [([], unitSquare)]
         let (ok, foldedPolys) = runState (simpleSolve1 polygon) initState
         if ok
           then do
             let unfoldedPolys = map applyTransform foldedPolys
             withFolded foldedPolys
             return $ withUnfolded unfoldedPolys
           else do
            return onFail
