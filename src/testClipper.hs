
module Main where

import Control.Monad
import Text.Parsec
import Text.Parsec.String
import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Parser
import Problem
import Clipper
import Draw

pTwoPolygons :: Parser (Polygon, Polygon)
pTwoPolygons = do
  p1 <- pPolygon
  p2 <- pPolygon
  return (p1, p2)

main :: IO ()
main = do
  text <- getContents
  case parse pTwoPolygons "<input>" text of
    Left e -> fail $ show e
    Right (p1,p2) -> do
      ps <- unionPolygons [p1] [p2]
      let dgram = mconcat $ map drawPolygon ps
      forM_ ps print
      mainWith dgram

