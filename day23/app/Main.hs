{-# LANGUAGE TupleSections #-}
module Main where

import Algorithm.Search (dijkstraAssoc)
import Data.Maybe (listToMaybe, catMaybes)
import Data.List (intercalate)
import Data.Char (isLetter)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

data Species = Am | Br | Co | De deriving (Show, Eq, Ord)
data YCoord = Hallway | RoomT | RoomB deriving (Show, Eq, Ord)
type Coord = (Int, YCoord)
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

inBounds :: Coord -> Bool
inBounds (x, _) = x >= lowerBound && x <= upperBound

parseFile :: String -> Board
parseFile s = foldl insertRoom M.empty rooms
  where letters = map toSpecies $ filter isLetter s
        roomContents = zip letters (drop 4 letters)
        rooms = zip [Am, Br, Co, De] roomContents
        insertRoom m (s, (rt, rb)) = (M.fromList [((roomX s, RoomT), rt), ((roomX s, RoomB), rb)]) `M.union` m

topOfRoom :: Board -> Int -> Maybe Coord
topOfRoom b x = listToMaybe $ filter (`M.member` b) [c1, c2]
  where c1 = (x, RoomT)
        c2 = (x, RoomB)

availableHallwaySpaces :: Board -> Int -> [Coord]
availableHallwaySpaces b sx = (exploreWith (+ 1) sx) ++ (exploreWith (+ (-1)) (sx - 1))
  where exploreWith f x | isRoomX x = exploreWith f (f x)
                        | not (inBounds (x, Hallway)) = []
                        | (x, Hallway) `M.member` b = []
                        | otherwise = (x, Hallway) : exploreWith f (f x)

pathToRoom :: Coord -> Species -> [Coord]
pathToRoom (sx, _) es | sx <= ex  = map (, Hallway) [sx + 1..ex]
                      | otherwise = map (, Hallway) [ex..sx - 1]
  where ex = roomX es

pathClear :: Board -> [Coord] -> Bool
pathClear b path = all (`M.notMember` b) path

toTopOfRoom :: YCoord -> Int
toTopOfRoom RoomT = 0
toTopOfRoom RoomB = 1

movingFromRoom :: Board -> Species -> [(Board, Int)]
movingFromRoom b s = case topOfRoom b (roomX s) of
                       Just (x, y) -> let withoutTop = (x, y) `M.delete` b
                                          extraCost = toTopOfRoom y
                                          Just movingOut = (x, y) `M.lookup` b
                                          in [(M.insert c movingOut withoutTop, (abs ((fst c) - x) + 1 + extraCost) * cost movingOut) | c <- availableHallwaySpaces b x]
                       Nothing -> []

roomPositions = [RoomT, RoomB]

movingIntoRoom :: Board -> [(Board, Int)]
movingIntoRoom b = concatMap attemptMoveToRoom [((c, Hallway), (c, Hallway) `M.lookup` b) | c <- [lowerBound..upperBound]]
  where attemptMoveToRoom (_, Nothing)  = []
        attemptMoveToRoom (c, Just s) | not clear = []
                                      | otherwise = [(b', (length p + 1 + extraCost) * (cost s))]
          where p = pathToRoom c s
                clear = roomClear && pathClear b p
                roomClear = ((roomX s, RoomT) `M.notMember` b) || ((roomX s, RoomB) `M.notMember` b)
                roomCorrect = all (== s) $ catMaybes [(roomX s, rp) `M.lookup` b | rp <- roomPositions]
                goToBottom = (roomX s, RoomB) `M.notMember` b
                extraCost = if goToBottom then 1 else 0
                c' = (roomX s, if goToBottom then RoomB else RoomT)
                b' = M.insert c' s $ M.delete c b

species = [Am, Br, Co, De]

nextMoves :: Board -> [(Board, Int)]
nextMoves b = (concatMap (movingFromRoom b) species) ++ movingIntoRoom b

isFinished :: Board -> Bool
isFinished b = all id [(M.lookup (roomX s, rp) b) == Just s | s <- species, rp <- [RoomT, RoomB]]

solve :: Board -> Maybe (Int, [Board])
solve = dijkstraAssoc nextMoves isFinished

printBoard :: Board -> String
printBoard b = intercalate "\n" [printLine l | l <- [Hallway, RoomT, RoomB]]
  where printLine l = [toChar ((x, l) `M.lookup` b) | x <- [lowerBound..upperBound]]
        toChar Nothing = ' '
        toChar (Just Am) = 'A'
        toChar (Just Br) = 'B'
        toChar (Just Co) = 'C'
        toChar (Just De) = 'D'

exampleInput :: Board
exampleInput = M.fromList [((0, Hallway), Am), ((1, RoomT), Br), ((1, RoomB), Am), ((2, Hallway), Br), ((4, Hallway), De)]

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
  -- putStrLn $ printBoard parsed
  -- putStrLn $ intercalate "\n---\n" $ map (printBoard . fst) $ nextMoves $ fst ((nextMoves parsed)!!0)
  let Just (cost, path) = solve parsed
  putStrLn $ intercalate "\n---\n" $ map printBoard path
  print path
  print cost
