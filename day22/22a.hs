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
cEmpty (Cuboid (sx, sy, sz) (ex, ey, ez)) = sx >= ex || sy >= ey || sz >= ez

cSubtract :: Cuboid -> Cuboid -> [Cuboid]
cSubtract a b | not (intersects a b) = [a]
              | otherwise = filter (not . cEmpty) cuboids
                 where (Cuboid (ax1, ay1, az1) (ax2, ay2, az2)) = a
                       (Cuboid (bx1, by1, bz1) (bx2, by2, bz2)) = b
                       (cx1, cy1, cz1) = (max ax1 bx1, max ay1 by1, max az1 bz1)
                       (cx2, cy2, cz2) = (min ax2 bx2, min ay2 by2, min az2 bz2)
                       cuboids = [
                         (Cuboid (ax1,ay1,az1) (ax2,ay2,cz1)),
                         (Cuboid (ax1,ay1,cz2) (ax2,ay2,az2)),
                         (Cuboid (cx1,ay1,cz1) (ax2,cy1,cz2)),
                         (Cuboid (cx2,cy1,cz1) (ax2,ay2,cz2)),
                         (Cuboid (ax1,cy2,cz1) (cx2,ay2,cz2)),
                         (Cuboid (ax1,ay1,cz1) (cx1,cy2,cz2))
                                 ]

cVolume :: Cuboid -> Int
cVolume (Cuboid (sx, sy, sz) (ex, ey, ez)) = vx * vy * vz
  where vx = abs (sx - ex) + 1
        vy = abs (sy - ey) + 1
        vz = abs (sz - ez) + 1

vUnion :: [Cuboid] -> Cuboid -> [Cuboid]
vUnion cs c = c : concatMap (`cSubtract` c) cs

vSubtract :: [Cuboid] -> Cuboid -> [Cuboid]
vSubtract cs c = concatMap (`cSubtract` c) cs

performCommand :: [Cuboid] -> Command -> [Cuboid]
performCommand cs (Command True c) = cs `vUnion` c
performCommand cs (Command False c) = cs `vSubtract` c

threshold = 50

interestedCuboid :: Cuboid
interestedCuboid = Cuboid ((-threshold), (-threshold), (-threshold)) (threshold, threshold, threshold)

nullCuboid :: Cuboid
nullCuboid = Cuboid (0, 0, 0) (0, 0, 0)

map3 :: (a -> b) -> (a, a, a) -> (b, b, b)
map3 f (a, b, c) = (f a, f b, f c)

cClamp :: Cuboid -> Cuboid
cClamp (Cuboid ss es) = Cuboid (sx', sy', sz') (ex', ey', ez')
  where (sx', sy', sz') = map3 (\x -> max x (-threshold)) ss
        (ex', ey', ez') = map3 (\x -> min x threshold) es

minifyCommand :: Command -> Command
minifyCommand (Command v cb) = Command v (cClamp cb)

iInBounds :: Int -> Bool
iInBounds x = x >= (-threshold) && x <= threshold

inBounds :: Coord -> Bool
inBounds (x, y, z) = ((maximum [x, y, z]) <= threshold) && ((minimum [x, y, z]) >= (-threshold))

main :: IO ()
main = do
  input <- readFile "./input_test"
  let (Right parsed) = parse parseFile "input" input
  let cmds = map minifyCommand parsed

  let end = foldl performCommand [] cmds
  let vol = foldr (+) 0 $ map cVolume end
  print vol
