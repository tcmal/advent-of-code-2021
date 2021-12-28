{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Algorithm.Search (dijkstraAssoc)
import qualified Data.Text as T
import Data.Maybe (listToMaybe, catMaybes)
import Data.List (intercalate, transpose)
import Data.Char (isLetter)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

data Species = Am | Br | Co | De deriving (Show, Eq, Ord)
type Coord = (Int, Int)
type Board = Map Coord Species

toSpecies :: Char -> Species
toSpecies 'A' = Am
toSpecies 'B' = Br
toSpecies 'C' = Co
toSpecies 'D' = De

cost :: Species -> Int
cost Am = 1
cost Br = 10
cost Co = 100
cost De = 1000

roomX :: Species -> Int
roomX Am = 1
roomX Br = 3
roomX Co = 5
roomX De = 7

isRoomX :: Int -> Bool
isRoomX 1 = True
isRoomX 3 = True
isRoomX 5 = True
isRoomX 7 = True
isRoomX _ = False

lowerBound = (-1)
upperBound = 9
lowestY = 4

inBounds :: Coord -> Bool
inBounds (x, _) = x >= lowerBound && x <= upperBound

parseFile :: String -> Board
parseFile s = foldl insertRoom M.empty rooms
  where letters = transpose $ map (map toSpecies . filter isLetter . T.unpack) $ T.splitOn "\n" (T.pack s)
        rooms = zip [Am, Br, Co, De] letters
        insertRoom m (s, cs) = (M.fromList [((roomX s, y), c) | (y, c) <- zip [1..] cs]) `M.union` m

topOfRoom :: Board -> Int -> Maybe Coord
topOfRoom b x = listToMaybe $ filter (`M.member` b) [(x, y) | y <- [1..lowestY]]

availableHallwaySpaces :: Board -> Int -> [Coord]
availableHallwaySpaces b sx = (exploreWith (+ 1) sx) ++ (exploreWith (+ (-1)) (sx - 1))
  where exploreWith f x | isRoomX x = exploreWith f (f x)
                        | not (inBounds (x, 0)) = []
                        | (x, 0) `M.member` b = []
                        | otherwise = (x, 0) : exploreWith f (f x)

pathToRoom :: Coord -> Species -> [Coord]
pathToRoom (sx, _) es | sx <= ex  = map (, 0) [sx + 1..ex]
                      | otherwise = map (, 0) [ex..sx - 1]
  where ex = roomX es

pathClear :: Board -> [Coord] -> Bool
pathClear b path = all (`M.notMember` b) path

toTopOfRoom :: Int -> Int
toTopOfRoom x = x - 1

movingFromRoom :: Board -> Species -> [(Board, Int)]
movingFromRoom b s = case topOfRoom b (roomX s) of
                       Just (x, y) -> let withoutTop = (x, y) `M.delete` b
                                          extraCost = toTopOfRoom y
                                          Just movingOut = (x, y) `M.lookup` b
                                          in [(M.insert c movingOut withoutTop, (abs ((fst c) - x) + 1 + extraCost) * cost movingOut) | c <- availableHallwaySpaces b x]
                       Nothing -> []

roomPositions = [1..lowestY]

movingIntoRoom :: Board -> [(Board, Int)]
movingIntoRoom b = concatMap attemptMoveToRoom [((c, 0), (c, 0) `M.lookup` b) | c <- [lowerBound..upperBound]]
  where attemptMoveToRoom (_, Nothing)  = []
        attemptMoveToRoom (c, Just s) | not clear = []
                                      | otherwise = [(b', (length p + 1 + extraCost) * (cost s))]
          where p = pathToRoom c s
                clear = pathClear b p && hasSpace && isCorrect
                occupants = catMaybes [(roomX s, y) `M.lookup` b | y <- roomPositions]
                hasSpace = length occupants < lowestY
                isCorrect = all (== s) occupants
                y' = lowestY - (length occupants)
                extraCost = toTopOfRoom y'
                c' = (roomX s, y')
                b' = M.insert c' s $ M.delete c b

species = [Am, Br, Co, De]

nextMoves :: Board -> [(Board, Int)]
nextMoves b = (concatMap (movingFromRoom b) species) ++ movingIntoRoom b

isFinished :: Board -> Bool
isFinished b = all id [(M.lookup (roomX s, rp) b) == Just s | s <- species, rp <- roomPositions]

solve :: Board -> Maybe (Int, [Board])
solve = dijkstraAssoc nextMoves isFinished

printBoard :: Board -> String
printBoard b = intercalate "\n" [printLine l | l <- [0..lowestY]]
  where printLine l = [toChar ((x, l) `M.lookup` b) | x <- [lowerBound..upperBound]]
        toChar Nothing = ' '
        toChar (Just Am) = 'A'
        toChar (Just Br) = 'B'
        toChar (Just Co) = 'C'
        toChar (Just De) = 'D'

printNexts :: [(Board, Int)] -> IO [()]
printNexts = sequence . map printNext

printNext :: (Board, Int) -> IO ()
printNext (b, c) = do
  putStrLn $ printBoard b
  print c

main :: IO ()
main = do
  input <- readFile "./input"
  let parsed = parseFile input
  let Just (cost, path) = solve parsed
  putStrLn $ intercalate "\n---\n" $ map printBoard path
  print path
  print cost
