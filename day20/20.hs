module Main where

import Data.List (foldl', intercalate)
import Data.Maybe (fromMaybe)

type Picture = ([[Bool]], Bool) -- Visible region, colour of all pixels not visible

readInteger :: [Bool] -> Int
readInteger = foldl' (\acc x -> acc * 2 + boolToDigit x) 0
  where boolToDigit True = 1
        boolToDigit False = 0

parseLine :: String -> [Bool]
parseLine = map charToBool
  where charToBool '#' = True
        charToBool '.' = False

(!?) :: [a] -> Int -> Maybe a
xs !? n | n < 0 || n >= (length xs) = Nothing
        | otherwise                 = Just (xs!!n)

getPixel :: Picture -> Int -> Int -> Bool
getPixel (p, d) x y = fromMaybe d $ (p !? y) >>= (!? x)

getNeighbours :: Picture -> Int -> Int -> [Bool]
getNeighbours p x y = [getPixel p (x + dx) (y + dy) | dy <- [(-1)..1], dx <- [(-1)..1]]

getNewPixel :: Picture -> [Bool] -> Int -> Int -> Bool
getNewPixel p alg x y = alg !! (readInteger $ getNeighbours p x y)

printPic :: Picture -> String
printPic = (intercalate "\n") . (map printLine) . fst 
  where printLine = map printBool
        printBool True = '#'
        printBool False = '.'

newDefault :: Bool -> [Bool] -> Bool
newDefault True alg = alg!!511
newDefault False alg = alg!!0

applyAlgorithm :: Picture -> [Bool] -> Picture
applyAlgorithm pic@(vis, def) alg = ([[getNewPixel pic alg x y | x <- [(-1)..mx]] | y <- [(-1)..my]], newDefault def alg)
  where mx = length (vis!!0) + 1
        my = length vis + 1

parseFile :: String -> ([Bool], Picture)
parseFile s = (parseLine alg, (map parseLine ls, False))
  where (alg:_:ls) = lines s

countPixels :: Picture -> Int
countPixels = foldr (+) 0 . map (length . filter id) . fst

main :: IO ()
main = do
  input <- readFile "./input"
  let (alg, pic) = parseFile input
  let pic2 = foldr (\_ p -> applyAlgorithm p alg) pic [1..2]
  putStrLn $ "Part 1: " ++ (show $ countPixels pic2)

  let pic50 = foldr (\_ p -> applyAlgorithm p alg) pic [1..50]
  putStrLn $ "Part 2: " ++ (show $ countPixels pic50)
