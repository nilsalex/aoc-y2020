{-# LANGUAGE BangPatterns #-}

module A05 (
             input
           , part_1
           , part_2
           , result_1
           , result_2
           ) where

import Data.List (sort)

input :: IO String
input = readFile "input/a05_1.txt"

toBinary :: String -> [Int]
toBinary = fmap replace
  where
    replace 'F' = 0
    replace 'B' = 1
    replace 'L' = 0
    replace 'R' = 1

binaryToInt :: [Int] -> Int
binaryToInt = go 0
  where
    go acc [] = acc
    go acc (d:ds) = let !acc' = 2*acc + d
                    in go acc' ds

parseInput :: String -> [Int]
parseInput = fmap (binaryToInt . toBinary) . lines

findGap :: [Int] -> Int
findGap (x:xs) = go x xs
  where
    go y (y':ys)
      | y+1 == y' = go y' ys
      | otherwise = y+1

part_1 :: [Int] -> Int
part_1 = maximum

part_2 :: [Int] -> Int
part_2 = findGap . sort

result_1 :: IO Int
result_1 = part_1 . parseInput <$> input

result_2 :: IO Int
result_2 = part_2 . parseInput <$> input
