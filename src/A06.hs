module A06 (
             input
           , part_1
           , part_2
           , result_1
           , result_2
           , parseInput
           ) where

import qualified Data.Set as S
import qualified Text.Parsec as P

input :: IO String
input = readFile "input/a06_1.txt"

parseInput :: String -> [[String]]
parseInput string = case P.parse groupsParser "input" string of
                      Left e       -> error (show e)
                      Right groups -> groups
  where
    lineParser   = do
                    line <- P.many1 P.letter
                    _    <- P.char '\n'
                    return line
    groupParser  = P.many1 lineParser
    groupsParser = P.sepBy1 groupParser (P.char '\n')

intersections :: (Foldable f, Ord a) => f (S.Set a) -> S.Set a
intersections = foldl1 S.intersection

part_1 :: [[String]] -> Int
part_1 = sum . fmap (S.size . S.unions . fmap S.fromList)

part_2 :: [[String]] -> Int
part_2 = sum . fmap (S.size . intersections . fmap S.fromList)

result_1 :: IO Int
result_1 = part_1 . parseInput <$> input

result_2 :: IO Int
result_2 = part_2 . parseInput <$> input
