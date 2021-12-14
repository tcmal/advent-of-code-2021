module Main where

import Data.List (sort)

isStartParen :: Char -> Bool
isStartParen '(' = True
isStartParen '[' = True
isStartParen '{' = True
isStartParen '<' = True
isStartParen _ = False

isEndParen :: Char -> Bool
isEndParen ')' = True
isEndParen ']' = True
isEndParen '}' = True
isEndParen '>' = True
isEndParen _ = False

flipParen :: Char -> Char
flipParen '(' = ')'
flipParen ')' = '('
flipParen '[' = ']'
flipParen ']' = '['
flipParen '{' = '}'
flipParen '}' = '{'
flipParen '<' = '>'
flipParen '>' = '<'
flipParen _ = undefined

getScore :: Char -> Int
getScore ')' = 3
getScore ']' = 57
getScore '}' = 1197
getScore '>' = 25137

completionScore :: Char -> Int
completionScore ')' = 1
completionScore ']' = 2
completionScore '}' = 3
completionScore '>' = 4
completionScore _ = undefined

consumeBlock :: Char -> String -> Either Char String
consumeBlock t "" = Right ""
consumeBlock t (c:cs) | c == flipParen t  = Right cs
                      | isStartParen c    = (consumeBlock c cs) >>= (consumeBlock t)
                      | otherwise         = Left c

corruptionScore :: String -> Int
corruptionScore "" = 0
corruptionScore (c:cs) = case consumeBlock c cs of
                      Left c -> getScore c
                      Right s -> corruptionScore s

getCompletion :: String -> String -> String
getCompletion stack "" = map flipParen stack
getCompletion stack (c:cs) | isStartParen c = getCompletion (c : stack) cs
                           | otherwise      = getCompletion (tail stack) cs

main :: IO ()
main = do
  s <- readFile "./input"
  let notCorrupted = filter ((== 0) . corruptionScore) (lines s)
  let scores = sort $ map ((foldl (\acc x -> (acc * 5) + (completionScore x)) 0) . getCompletion "") notCorrupted

  print $ scores !! (div (length scores) 2)
