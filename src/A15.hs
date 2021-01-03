{-# LANGUAGE BangPatterns #-}

module A15 ( input
           , part_1
           , part_2
           , result_1
           , result_2
           ) where

import qualified Data.IntMap.Strict as I

input :: IO String
input = readFile "input/a15_1.txt"

result_1 :: IO Int
result_1 = part_1 . parseInput <$> input

result_2 :: IO Int
result_2 = part_2 . parseInput <$> input

parseInput :: String -> [Int]
parseInput = fmap read . lines . fmap (\c -> case c of ',' -> '\n'; _ -> c)

data State = State
    { lastTurn   :: !Int
    , lastSpoken :: !Int
    , history    :: !(I.IntMap Int)
    }
  deriving Show

turn :: State -> State
turn (State t l h) = State t' l' h'
  where
    !t' = t+1

    !previous = I.lookup l h

    !l'       = case previous of
                  Nothing -> 0
                  Just p  -> t - p

    !h'       = I.insert l t h

initialState :: [Int] -> State
initialState xs = State (length xs) (last xs) (I.fromList (zip (init xs) [1..]))

iterateN :: (a -> a) -> Int -> a -> a
iterateN f = go
  where
    go 0  !s = s
    go !n !s = go (n-1) (f s)

part_1 :: [Int] -> Int
part_1 xs = lastSpoken . iterateN turn (2020 - n) . initialState $ xs
  where
    n = length xs

part_2 :: [Int] -> Int
part_2 xs = lastSpoken . iterateN turn (30000000 - n) . initialState $ xs
  where
    n = length xs
