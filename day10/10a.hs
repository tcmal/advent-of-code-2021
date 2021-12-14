module Main where

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

main :: IO ()
main = do
  s <- readFile "./input"
  let notCorrupted = foldr (+) 0 $ map corruptionScore (lines s)
  print notCorrupted
