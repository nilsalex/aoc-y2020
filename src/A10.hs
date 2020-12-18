{-# LANGUAGE BangPatterns #-}

module A10 ( input
           , part_1
           , part_2
           , result_1
           , result_2
           ) where

import Data.List      (sort)
import Data.Bifunctor (first,second)

input :: IO String
input = readFile "input/a10_1.txt"

result_1 :: IO Int
result_1 = part_1 . parseInput <$> input

result_2 :: IO Integer
result_2 = part_2 . parseInput <$> input

parseInput :: String -> [Int]
parseInput = fmap read . lines

canonicalise :: [Int] -> [Int]
canonicalise xs = sort $ 0 : (maximum xs + 3) : xs

diffs :: [Int] -> [Int]
diffs (x:y:xs) = y-x : diffs (y:xs)
diffs _        = []

hops :: [Int] -> [Int]
hops [_]    = []
hops (x:xs) = n : hops xs
  where
    !n = countWhile (\y -> y-x <= 3) xs

countWhile :: (a -> Bool) -> [a] -> Int
countWhile = go 0
  where
    go !n pred (x:xs) = if   pred x
                        then go (n+1) pred xs
                        else n
    go !n _ _         = n


applyFilter :: Num a => [Bool] -> [a] -> [a]
applyFilter = zipWith (\f x -> if f then x else 0)

countPaths :: [Int] -> [Integer]
countPaths = reverse . countPathsReverse . reverse

countPathsReverse :: [Int] -> [Integer]
countPathsReverse rhs = counts
  where
    filter1 = (>0) <$> drop 1 rhs
    filter2 = (>1) <$> drop 2 rhs
    filter3 = (>2) <$> drop 3 rhs

    counts  = zipWith3 (\a b c -> a+b+c)
                       (1:    applyFilter filter1 counts)
                       (0:0:  applyFilter filter2 counts)
                       (0:0:0:applyFilter filter3 counts)

part_1 :: [Int] -> Int
part_1 xs = ones * threes
  where
    xs'    = canonicalise xs
    ds     = diffs xs'
    ones   = length $ filter (==1) ds
    threes = length $ filter (==3) ds

part_2 :: [Int] -> Integer
part_2 xs = head $ countPaths hs
  where
    xs'     = canonicalise xs
    hs      = hops xs'
