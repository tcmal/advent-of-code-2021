{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import Data.Set (Set)
import Data.List
import qualified Data.Set as S

type Coord = (Int, Int)
data FoldInstr = Fold Axis Int deriving (Show, Eq, Ord)
data Axis = XAxis | YAxis deriving (Show, Eq, Ord)

parseCoords :: String -> Coord
parseCoords s = (read x, read y)
  where [x, y] = map T.unpack $ T.splitOn "," (T.pack s)

parseFold :: String -> FoldInstr
parseFold (stripPrefix "fold along y=" -> Just cs) = Fold YAxis (read cs)
parseFold (stripPrefix "fold along x=" -> Just cs) = Fold XAxis (read cs)

parseFile :: String -> (Set Coord, [FoldInstr])
parseFile s = (S.fromList $ map parseCoords (lines coordSection), map parseFold (lines foldSection))
  where [coordSection, foldSection] = map T.unpack $ T.splitOn "\n\n" (T.pack s)

performFold :: Set Coord -> FoldInstr -> Set Coord
performFold coords (Fold YAxis ye) = S.map (\(x, y) -> (x, -(abs (y - ye)) + ye)) coords
performFold coords (Fold XAxis xe) = S.map (\(x, y) -> (-(abs (x - xe)) + xe, y)) coords

main :: IO ()
main = do
  f <- readFile "./input"
  let (coords, instrs) = parseFile f;
  print $ length $ performFold coords (head instrs)
