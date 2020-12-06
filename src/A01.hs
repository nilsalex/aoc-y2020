module A01 (
             input
           , part_1
           , part_2
           , result_1
           , result_2
           ) where

input :: IO [Int]
input = fmap read . lines <$> readFile "input/a01_1.txt"

part_1 :: [Int] -> Int
part_1 xs = p
  where
    (_,p) : _ = filter ((== 2020) . fst) $
                (\a b -> (a+b, a*b)) <$> xs <*> xs

part_2 :: [Int] -> Int
part_2 xs = p
  where
    (_,p) : _ = filter ((== 2020) . fst) $
                (\a b c -> (a+b+c, a*b*c)) <$> xs <*> xs <*> xs

result_1 :: IO Int
result_1 = part_1 <$> input

result_2 :: IO Int
result_2 = part_2 <$> input
