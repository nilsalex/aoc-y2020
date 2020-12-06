module A03 (
             input
           , part_1
           , part_2
           , result_1
           , result_2
           ) where

import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Data.List (unfoldr)

input :: IO String
input = readFile "input/a03_1.txt"

data Field = Space | Tree deriving Show
type Pos = (Int, Int)

parseInput :: String -> (Int, Int, S.Set Pos)
parseInput s = (width, height, S.fromList positions)
  where
    ls     = lines s
    width  = length . head $ ls
    height = length ls

    positionsInLine y line =
      mapMaybe (\(x,c) -> case c of
                            '#' -> Just (x,y)
                            _   -> Nothing) $
               zip [0..width-1] line

    positions = concat $ zipWith positionsInLine [0..height-1] ls

treesInDir :: S.Set Pos -> Int -> Int -> Int -> Int -> Int
treesInDir trees width height dx dy =
    length $ filter (`S.member` trees) coords
  where
    coords = takeWhile ((< height) . snd) $
             fmap (\i -> (i*dx `mod` width, i*dy)) [0..]

part_1 :: S.Set Pos -> Int -> Int -> Int
part_1 trees width height = treesInDir trees width height 3 1

part_2 :: S.Set Pos -> Int -> Int -> Int
part_2 trees width height = product treesInDirs
  where
    treesInDirs =
      fmap (uncurry (treesInDir trees width height)) slopes
    slopes = [ (1,1)
             , (3,1)
             , (5,1)
             , (7,1)
             , (1,2)
             ]

result_1 :: IO Int
result_1 = do
  (width,height,trees) <- parseInput <$> input
  return $ part_1 trees width height

result_2 :: IO Int
result_2 = do
  (width,height,trees) <- parseInput <$> input
  return $ part_2 trees width height
