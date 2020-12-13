module A09 ( input
           , part_1
           , part_2
           , result_1
           , result_2
           ) where

import Data.Maybe (mapMaybe)
import Data.List  (tails)

input :: IO String
input = readFile "input/a09_1.txt"

result_1 :: IO Integer
result_1 = part_1 . parseInput <$> input

result_2 :: IO Integer
result_2 = part_2 . parseInput <$> input

parseInput :: String -> [Integer]
parseInput = fmap read . lines

isValidChunk :: [Integer] -> Bool
isValidChunk xs = last xs `elem` sums (init xs)

sums :: [Integer] -> [Integer]
sums []       = []
sums (x:xs)   = fmap (x+) xs ++ sums xs

sumsTo :: Integer -> [Integer] -> Maybe [Integer]
sumsTo _ [] = Nothing
sumsTo target (x:xs) = case x `compare` target of
                         LT -> (x:) <$> sumsTo (target-x) xs
                         EQ -> Just [x]
                         GT -> Nothing

part_1 :: [Integer] -> Integer
part_1 xs = go xs
  where
    go xs
      | isValidChunk (take 26 xs) = go (tail xs)
      | otherwise       = xs !! 25

part_2 :: [Integer] -> Integer
part_2 xs = minimum summands + maximum summands
  where
    target     = part_1 xs
    summands:_ = mapMaybe (sumsTo target) (tails xs)
