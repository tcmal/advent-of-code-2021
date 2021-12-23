{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid (Endo (Endo), appEndo)
import Data.Maybe (listToMaybe, isJust)
import Data.List (isPrefixOf)
import Linear.Vector ((^+^), (^-^))
import Linear.V3 (V3 (V3))

import Data.Set (Set)

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T

type Vec3 = V3 Integer
type Transform = Endo Vec3
data Scanner = Scanner { beacons :: [Vec3]
                       } deriving (Show, Eq)
data PositionedScanner = PositionedScanner { scanner :: Scanner
                                            ,originOffset :: Vec3
                                           } deriving (Show)

instance Show Transform where
  show c = show $ appEndo c (V3 0 0 0)

nullTrans = Endo id
rotX = Endo \(V3 x y z) -> V3    x (- z)   y
rotY = Endo \(V3 x y z) -> V3    z    y (- x)
rotZ = Endo \(V3 x y z) -> V3 (- y)   x    z
translate v = Endo (v ^+^)

rotations :: [Transform]
rotations = [a <> b | a <- ras, b <- rbs]
  where ras = [ nullTrans, rotY, rotY <> rotY, rotY <> rotY <> rotY
              , rotZ, rotZ <> rotZ <> rotZ]
        rbs = [nullTrans, rotX, rotX <> rotX, rotX <> rotX <> rotX]

threshold :: Integer
threshold = 12

firstJust :: [Maybe a] -> Maybe a
firstJust xs | null js = Nothing
             | otherwise = (head js)
  where js = filter isJust xs

parseFile :: String -> [[Vec3]]
parseFile s = reverse $ parseLines (tail $ lines s) [[]]
  where parseLines [] cs = cs
        parseLines (l:ls) (c:cs) | "---" `isPrefixOf` l = parseLines ls ([] : c : cs)
                                 | null l               = parseLines ls (c : cs)
                                 | otherwise            = parseLines ls ((parseLine l : c) : cs)
        parseLine l = V3 x y z
          where [x, y, z] = map (read . T.unpack) $ T.splitOn "," $ T.pack l

commonOffset :: [Vec3] -> [Vec3] -> Maybe Vec3
commonOffset ys xs = listToMaybe aboveThreshold >>= (Just . fst)
  where dists = [x ^-^ y | x <- xs, y <- ys]
        distCounts = M.toList $ M.fromListWith (+) [(d, 1) | d <- dists]
        aboveThreshold = filter ((>= threshold) . snd) distCounts

applyTransform :: Transform -> Scanner -> Scanner
applyTransform t (Scanner bs) = Scanner (map (appEndo t) bs)

-- attempt to get a's offset from b
offsetFrom :: Scanner -> Scanner -> Maybe (Vec3, Scanner)
offsetFrom a b = listToMaybe successes
  where attempts = [attemptWith rot | rot <- rotations]
        successes = [(a, b) | (Just a, b) <- attempts]
        attemptWith rot = (commonOffset (beacons a') (beacons b), a')
          where a' = applyTransform rot a

adjustedOffsetFrom :: PositionedScanner -> Scanner -> Maybe PositionedScanner
adjustedOffsetFrom b a = case a `offsetFrom` (scanner b) of
                           Just (off, sc) -> Just $ PositionedScanner sc (off ^+^ (originOffset b))
                           Nothing        -> Nothing

solveMore :: [PositionedScanner] -> [Scanner] -> ([PositionedScanner], [Scanner])
solveMore ks us = foldr solveOne (ks, us) us
  where solveOne s (ks', us') = case firstJust (map (\k -> adjustedOffsetFrom k s) ks') of
                                  Just d -> (d : ks', filter (/= s) us')
                                  Nothing -> (ks', us')

calcAllOffsets :: [Scanner] -> [PositionedScanner]
calcAllOffsets (s:ss) = keepSolvingMore ([PositionedScanner s (V3 0 0 0)], ss)
  where keepSolvingMore (ks,[]) = ks
        keepSolvingMore (ks,us) = keepSolvingMore (solveMore ks us)

absoluteBeacons :: PositionedScanner -> Set Vec3
absoluteBeacons (PositionedScanner sc pos) = S.fromList $ map (pos ^+^) (beacons sc)

manhattan :: Vec3 -> Vec3 -> Integer
manhattan (V3 x y z) (V3 x' y' z') = (abs (x' -x)) + (abs (y' - y)) + (abs (z' - z))

main :: IO ()
main = do
  input <- readFile "./input"
  let parsed = map Scanner $ parseFile input
  let positioned = calcAllOffsets parsed
  let beacons = foldr S.union S.empty $ map absoluteBeacons positioned

  print $ "Part 1: " ++ (show $ S.size beacons)

  let scannerPositions = map originOffset positioned
  print $ "Part 2: " ++ (show $ maximum [manhattan a b | a <- scannerPositions, b <- scannerPositions])
