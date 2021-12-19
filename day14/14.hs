{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T

type Pair = (Char, Char)

doSubstitutions :: Map Pair Char -> Map Pair Int -> Map Pair Int
doSubstitutions rules s = M.fromListWith (+) $ concatMap checkPair (M.toList s)
  where checkPair ((a, b), count) = case M.lookup (a, b) rules of
                             Just v -> [((a, v), count), ((v, b), count)]
                             Nothing -> [((a, b), count)]

stringToPairs :: String -> Map Pair Int
stringToPairs s = M.fromListWith (+) $ [((a, b), 1) | (a, b) <- (zip s (tail s))] ++ [((last s, '_'), 1)]

countLetters :: Map Pair Int -> Map Char Int
countLetters m = M.fromListWith (+) [(a, count) | ((a, _), count) <- M.toList m]

parseRuleEntry :: String -> (Pair, Char)
parseRuleEntry s = ((a, b), head $ T.unpack r)
  where [t, r] = T.splitOn " -> " (T.pack s)
        [a, b] = T.unpack t

parseFile :: String -> (Map Pair Int, Map Pair Char)
parseFile s = (stringToPairs (T.unpack initial), M.fromList $ map parseRuleEntry rules)
  where [initial, rulesSection] = T.splitOn "\n\n" (T.pack s)
        rules = lines (T.unpack rulesSection)


main  :: IO ()
main = do
  input <- readFile "./input"
  let (initial, rules) = parseFile input
  let result = foldr (\_ acc -> doSubstitutions rules acc) initial [1..40]
  let counts = countLetters result

  let mx = maximum (M.elems counts)
  let mi = minimum (M.elems counts)
  print (mx, mi)
  print $ mx - mi
