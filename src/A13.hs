{-# LANGUAGE BangPatterns #-}

module A13 ( input
           , part_1
           , part_2
           , result_1
           , result_2
           ) where

import Data.List      (minimumBy,foldl')

input :: IO String
input = readFile "input/a13_1.txt"

result_1 :: IO Int
result_1 = part_1 . parseInput <$> input

result_2 :: IO Integer
result_2 = part_2 . parseInput <$> input

parseInput :: String -> (Int,[(Int,Int)])
parseInput s = (time, ids)
  where
    l1:l2:_ = lines s
    time    = read l1

    ids     = go [] [] 0 l2

    go acc [] _ []           = acc
    go acc buf !count []     = (read (reverse buf), count) : acc
    go acc buf !count (x:xs) =
      case x of
        ',' -> case buf of
                 [] -> go acc [] (count+1) xs
                 _  -> go ((read (reverse buf), count) : acc) [] (count+1) xs
        'x' -> go acc [] count xs
        _   -> go acc (x:buf) count xs

euclid :: Int -> Int -> Int
euclid a b = go (max a b) (min a b)
  where
    go !a 0  = a
    go !a !b = go b (a `mod` b)

euclidPlus :: Integer -> Integer -> (Integer, Integer, Integer)
euclidPlus a b
  | a < b = let (q,x,y) = euclidPlus' b a
            in (q,y,x)
  | otherwise = euclidPlus' a b

euclidPlus' :: Integer -> Integer -> (Integer, Integer, Integer)
euclidPlus' a b = go a b 1 0 0 1
  where
    go !a 0 !r _ !t _    = (a,r,t)
    go !a !b !r !s !t !u = go b (a-q*b) s (r-q*s) u (t-q*u)
      where
        q = a `div` b

reduce :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
reduce (a,m) (b,n)
    | (m-n) `mod` q /= 0 = error "unsolvable"
    | otherwise          = (a*b,o)
  where
    (q,_,y') = euclidPlus b a
    y        = negate y'
    o'       = m + a * y * ((m-n) `div` q)
    o        = o' `mod` (a*b)

part_1 :: (Int,[(Int,Int)]) -> Int
part_1 (time,ids) = i*m
  where
    mods = fmap (\(i,_) -> (i,(-time) `mod` i)) ids
    (i,m) = minimumBy (\(_,m') (_,m'') -> m' `compare` m'') mods

part_2 :: (Int,[(Int,Int)]) -> Integer
part_2 (_,xs) = snd $ foldl' reduce x xs'
  where
    (x:xs') = reverse . fmap (\(a,m) -> (fromIntegral a, fromIntegral $ (-m) `mod` a)) $ xs
