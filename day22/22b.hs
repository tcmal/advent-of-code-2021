module Main where

import Text.Parsec
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace (trace)

type Coord = (Int, Int, Int)
data Cuboid = Cuboid Coord Coord deriving (Show, Eq)
data Command = Command Bool Cuboid deriving (Show, Eq)

onOff = toBool <$> (try (string "on") <|> string "off")
  where toBool "on" = True
        toBool _ = False

number = do
  sign <- (option '0' (char '-'))
  num <- many1 digit
  return $ read (sign : num)

range = do
  start <- number
  string ".."
  end <- number

  return $ (start, end)

parseLine = do
  val <- onOff
  string " x="
  (sx, ex) <- range
  string ",y="
  (sy, ey) <- range
  string ",z="
  (sz, ez) <- range

  let c = Cuboid (sx, sy, sz) (ex, ey, ez)
  return $ Command val c
parseFile = endBy parseLine (char '\n')

intersects1D :: (Int, Int) -> (Int, Int) -> Bool
intersects1D (a1, a2) (b1, b2) = a2 > b1 && b2 > a1

zipCoords :: Coord -> Coord -> [(Int, Int)]
zipCoords (x, y, z) (x', y', z') = [(x, x'), (y, y'), (z, z')]

intersects :: Cuboid -> Cuboid -> Bool
intersects (Cuboid s e) (Cuboid s' e') = any (uncurry intersects1D) (zip (zipCoords s e) (zipCoords s' e'))

cEmpty :: Cuboid -> Bool
cEmpty (Cuboid (sx, sy, sz) (ex, ey, ez)) = sx > ex || sy > ey || sz > ez

cVolume :: Cuboid -> Int
cVolume (Cuboid (sx, sy, sz) (ex, ey, ez)) = vx * vy * vz
  where vx = abs (sx - (ex + 1))
        vy = abs (sy - (ey + 1))
        vz = abs (sz - (ez + 1))

ccVolume :: ChargedCuboid -> Int
ccVolume c | charge c = cVolume (cuboid c)
           | otherwise = -(cVolume (cuboid c))

cIntersection :: Cuboid -> Cuboid -> Cuboid
cIntersection c1@(Cuboid s1 e1) c2@(Cuboid s2 e2) | not  $ intersects c1 c2 = nullCuboid
                                                  | otherwise = (Cuboid s' e')
                                                    where s' = map3 (uncurry max) (zip3t s1 s2)
                                                          e' = map3 (uncurry min) (zip3t e1 e2)

data ChargedCuboid = ChargedCuboid { charge :: Bool
                                    ,cuboid :: Cuboid
                                    } deriving (Show, Eq)

performCommand :: [ChargedCuboid] -> Command -> [ChargedCuboid]
performCommand cs (Command val c) | val = (ChargedCuboid val c) : (cs ++ overlaps)
                                  | otherwise = cs ++ overlaps
  where overlaps = filter (not . cEmpty . cuboid) $ map invertedIntersection cs
        invertedIntersection cc = ChargedCuboid (not (charge cc)) ((cuboid cc) `cIntersection` c)

nullCuboid :: Cuboid
nullCuboid = Cuboid (0, 0, 0) ((-1), (-1), (-1))

map3 :: (a -> b) -> (a, a, a) -> (b, b, b)
map3 f (a, b, c) = (f a, f b, f c)

zip3t :: (a, a, a) -> (b, b, b) -> ((a, b), (a, b), (a, b))
zip3t (x, y, z) (x', y', z') = ((x, x'), (y, y'), (z, z'))

main = do
  input <- readFile "./input"
  let (Right cmds) = parse parseFile "input" input

  let cs = foldl performCommand [] cmds
  let vol = foldr (+) 0 $ map ccVolume cs
  print vol
