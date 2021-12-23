{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Monoid (Endo (Endo), appEndo)
import Data.Maybe (isJust)
import Data.List (isPrefixOf)
import Linear.Vector ((^+^))
import Linear.V3 (V3 (V3), cross)
import Linear.Matrix ((!*))
import Data.Set (Set)
import Data.List (find)
import Control.Applicative ((<|>))
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T

type Vec3 = V3 Integer
type Coords = [Vec3]
type CoordsWithOffset = (Coords, Vec3)
type Transform = Endo Vec3

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

firstJust :: [Maybe a] -> Maybe a
firstJust ms | null js = Nothing
             | otherwise = head js
  where js = filter isJust ms

threshold :: Integer
threshold = 12

attemptFindOffset :: Coords -> Coords -> Maybe Vec3
attemptFindOffset base other = (find ((>= threshold) . snd) offsetCounts) >>= (Just . fst)
  where allOffsets = zipWith (-) base other
        offsetCounts = M.toList $ M.fromListWith (+) [(x, 1) | x <- allOffsets]

attemptFindOffsetFrom :: [CoordsWithOffset] -> Coords -> Maybe (Vec3, Coords)
attemptFindOffsetFrom [] v = Nothing
attemptFindOffsetFrom ((base, baseOff):cs) ds = (firstJust foundFromOriented) <|> attemptFindOffsetFrom cs ds
  where absoluteOffset offset = baseOff + offset
        orientedDs = [map (appEndo o) ds | o <- rotations]
        foundFromOriented = [(absoluteOffset <$> attemptFindOffset base d) >>= (Just . (, d)) | d <- orientedDs]

discoverMoreOffsets :: [CoordsWithOffset] -> [Coords] -> ([CoordsWithOffset], [Coords])
discoverMoreOffsets [] (c:cs) = ([(c, V3 0 0 0)], cs)
discoverMoreOffsets discovered cs = (discovered', undiscovered)
  where discoverAttempts = zip cs (map (attemptFindOffsetFrom discovered) cs)
        discovered' = discovered ++ [(d', off) | (_, Just (off, d')) <- discoverAttempts]
        undiscovered = [a | (a, Nothing) <- discoverAttempts]

parseFile :: String -> [Coords]
parseFile s = reverse $ parseLines (tail $ lines s) [[]]
  where parseLines [] cs = cs
        parseLines (l:ls) (c:cs) | "---" `isPrefixOf` l = parseLines ls ([] : c : cs)
                                 | null l               = parseLines ls (c : cs)
                                 | otherwise            = parseLines ls ((parseLine l : c) : cs)
        parseLine l = V3 x y z
          where [x, y, z] = map (read . T.unpack) $ T.splitOn "," $ T.pack l

repeatedApply :: Eq a => Eq b => (a -> b -> (a, b)) -> a -> b -> (a, b)
repeatedApply f a b | (a', b') == (a, b) = (a', b')
                    | otherwise          = repeatedApply f a' b'
  where (a', b') = f a b

main :: IO ()
main = do
  input <- readFile "./input_test"
  let parsed = parseFile input
  let (offsets, remaining) = repeatedApply discoverMoreOffsets [] parsed
  print (length remaining)
  print $ map snd offsets
