
module Parser where

import Control.Monad
import Data.Ratio

import Text.Parsec
import Text.Parsec.String

import Problem

integer :: Parser Int
integer = do
  nstr <- many1 digit
  endOfLine
  return $ read nstr

number :: Parser Number
number = do
  nstr <- many1 digit
  maybeMstr <- optionMaybe $ do
                 char '/'
                 many1 digit
  case maybeMstr of
    Nothing -> return $ fromIntegral $ read nstr
    Just mstr -> return $ read nstr % read mstr

endByNewline :: Parser a -> Parser a
endByNewline p = do
  res <- p
  optional $ try endOfLine
  return res

pPoint :: Parser Point
pPoint = do
  x <- number
  char ','
  y <- number
  return (x,y)

pProblem :: Parser Problem
pProblem = do
  nPolygons <- integer
  polygons <- replicateM nPolygons (endByNewline pPolygon)
  nSegments <- integer
  skeleton <- replicateM nSegments (endByNewline pSegment)
  return $ Problem polygons skeleton

pPolygon :: Parser Polygon
pPolygon = do
  n <- integer
  replicateM n (endByNewline pPoint)

pSegment :: Parser Segment
pSegment = do
  start <- pPoint
  char ' '
  end <- pPoint
  return (start, end)

parseProblem :: FilePath -> IO Problem
parseProblem path = do
  r <- parseFromFile pProblem path
  case r of
    Left err -> fail $ show err
    Right problem -> return problem

