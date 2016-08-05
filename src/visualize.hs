
module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
-- import System.Environment

import Problem
import Draw
import Parser

mkDiagram :: FilePath -> IO (Diagram B)
mkDiagram path = do
  problem <- parseProblem path
  return $ drawProblem problem

main :: IO ()
main = mainWith mkDiagram
