
module Main where

import Control.Monad
import Data.Ratio
import Diagrams.Backend.SVG
import Diagrams.Prelude
import System.Environment

import Problem
import Draw
import Parser

main :: IO ()
main = do
  [path] <- getArgs
  problem <- parseProblem path

  let points = concat $ map (\(p1, p2) -> [p1, p2]) $ pSkeleton problem

  let drawing = (# lc red) $ foldr1 atop $ map drawSegment $ pSkeleton problem

  forM_ (zip [1..] points) $ \(i, point) -> do
    let segments = filter
                     (\(p1, p2) -> p1 == point || p2 == point)
                     (pSkeleton problem)
    let segmentsD = (# lc black) $ foldr1 atop $ map drawSegment segments

    let trect = rect 1.5 0.2 # lc white
    let coordinates =
          (   (text (show $ fst point) # fontSize 100 # lc black <> trect)
          === (text (show $ snd point) # fontSize 100 # lc black <> trect)
          ) # moveTo (mkPoint (1%2, (-1)%10))

    renderSVG
      ("point_" ++ show i ++ ".svg")
      (mkWidth 2000)
      ((coordinates === (segmentsD `atop` drawing)) # bg white # bgFrame 0.1 white)

  print "That's all, folks!"
