module A02 (
             input
           , part_1
           , part_2
           , result_1
           , result_2
           ) where

import qualified Text.Parsec as P

input :: IO String
input = readFile "input/a02_1.txt"

data Policy = Policy Int Int Char deriving Show

type Password = String

policyParser :: P.Parsec String st Policy
policyParser = do
   min <- read <$> P.many1 P.digit
   _   <- P.char '-'
   max <- read <$> P.many1 P.digit
   _   <- P.char ' '
   Policy min max <$> P.letter

lineParser :: P.Parsec String st (Policy, Password)
lineParser = do
  policy   <- policyParser
  _        <- P.char ':'
  _        <- P.char ' '
  password <- P.many P.letter
  _        <- P.char '\n'
  return (policy, password)

inputParser :: P.Parsec String st [(Policy, Password)]
inputParser = P.many1 lineParser

validate_1 :: Policy -> Password -> Bool
validate_1 (Policy min max c) = go 0
  where
    go count [] = count >= min
    go count (x:xs)
      | x == c = let count' = count+1
                 in case count' `compare` max of
                      GT -> False
                      _  -> go count' xs
      | otherwise = go count xs

validate_2 :: Policy -> Password -> Bool
validate_2 (Policy pos1 pos2 c) password = (char1 == c) `xor` (char2 == c)
  where
    char1 = password !! (pos1 - 1)
    char2 = password !! (pos2 - 1)
    xor a b = a /= b

part_1 :: [(Policy, Password)] -> Int
part_1 = length . filter (uncurry validate_1)

part_2 :: [(Policy, Password)] -> Int
part_2 = length . filter (uncurry validate_2)

parseInput :: String -> [(Policy, Password)]
parseInput s = case P.parse inputParser "input" s of
                 Left e   -> error $ show e
                 Right ps -> ps

result_1 :: IO Int
result_1 = part_1 . parseInput <$> input

result_2 :: IO Int
result_2 = part_2 . parseInput <$> input
