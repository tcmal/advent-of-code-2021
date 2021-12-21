module Main where

import Numeric (readHex)
import Text.Printf (printf)
import Data.Char (digitToInt)
import Data.List (foldl')


readInt :: Int -> [Bool] -> (Integer, [Bool])
readInt n bs = (foldl' (\acc x -> acc * 2 + boolToDigit x) 0 (take n bs), drop n bs)
  where boolToDigit True = 1
        boolToDigit False = 0

hexToBits :: String -> [Bool]
hexToBits "" = []
hexToBits (c:cs) = case readHex [c] of
                (x, _):_ -> (map binToBool $ printf "%04b" (x :: Int)) ++ hexToBits cs
                _ -> []
  where binToBool '1' = True
        binToBool _ = False

data Packet = Literal Integer Integer Integer |
              Operator Integer Integer [Packet] |
              Padding
            deriving (Show, Eq, Ord)


parseHexPacket :: String -> (Packet, [Bool])
parseHexPacket = parsePacket . hexToBits

parseHeader :: [Bool] -> (Integer, Integer, [Bool])
parseHeader bs = (version, typ, body)
  where (version, afterVersion) = readInt 3 bs
        (typ, body) = readInt 3 afterVersion

parsePacket :: [Bool] -> (Packet, [Bool])
parsePacket bs = case parseHeader bs of
                   (ver, 4, body)  -> let (val, rest) = readLiteralInner body
                                          in (Literal ver 4 val, rest)
                   (ver, op, body) -> let (val, rest) = readOpInner body
                                          in (Operator ver op val, rest)

readLiteralInner :: [Bool] -> (Integer, [Bool])
readLiteralInner bs = readLiteralBlock 0 bs
  where readLiteralBlock acc (cont:bs) | cont      = readLiteralBlock acc' rest
                                       | otherwise = (acc', rest)
                                       where (val, rest) = readInt 4 bs
                                             acc' = (16 * acc) + val

readOpInner :: [Bool] -> ([Packet], [Bool])
readOpInner (False:header) = (parseAllPackets (take (fromIntegral len) body), drop (fromIntegral len) body)
  where (len, body) = readInt 15 header
        parseAllPackets [] = []
        parseAllPackets bs = let (parsed, remaining) = parsePacket bs
                                 in parsed : parseAllPackets remaining
readOpInner (True:header) = parseNPackets n body
  where (n, body) = readInt 11 header
        parseNPackets 0 b = ([], b)
        parseNPackets n bs = let (parsed, remaining) = parsePacket bs
                                 (restParsed, endRemaining) = parseNPackets (n - 1) remaining
                                 in (parsed : restParsed, endRemaining)

sumVersion :: Packet -> Integer
sumVersion (Literal v _ _) = v
sumVersion (Operator v _ ps) = v + (foldr (+) 0 (map sumVersion ps))
sumVersion Padding = 0

eval :: Packet -> Integer
eval (Literal _ _ v) = v
eval (Operator _ 0 ps) = sum (map eval ps)
eval (Operator _ 1 ps) = product (map eval ps)
eval (Operator _ 2 ps) = minimum (map eval ps)
eval (Operator _ 3 ps) = maximum (map eval ps)
eval (Operator _ 5 ps) = if a > b then 1 else 0
  where [a, b] = map eval ps
eval (Operator _ 6 ps) = if a < b then 1 else 0
  where [a, b] = map eval ps
eval (Operator _ 7 ps) = if a == b then 1 else 0
  where [a, b] = map eval ps


main :: IO ()
main = do
  input <- readFile "./input"
  let (parsed, _) = parseHexPacket input
  print $ sumVersion parsed
  print $ eval parsed
