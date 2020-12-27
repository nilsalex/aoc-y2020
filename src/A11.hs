{-# LANGUAGE BangPatterns #-}

module A11 ( input
           , part_1
           , part_2
           , result_1
           , result_2
           ) where

import Control.Monad (join)
import Data.List      (sort)
import Data.Bifunctor (first,second)
import qualified Data.Map.Strict as M

input :: IO String
input = readFile "input/a11_1.txt"

result_1 :: IO Int
result_1 = part_1 . parseInput <$> input

result_2 :: IO Int
result_2 = part_2 . parseInput <$> input

data Field = EMPTY
           | CHAIR
           | PERSON
        deriving (Show, Eq)

isNotPerson :: Field -> Bool
isNotPerson PERSON = False
isNotPerson _     = True

isPerson :: Field -> Bool
isPerson PERSON = True
isPerson _     = False

parseInput :: String -> [[Field]]
parseInput = fmap (fmap toField) . lines
  where
    toField '.' = EMPTY
    toField 'L' = CHAIR
    toField '#' = PERSON

addBoundary :: [[Field]] -> [[Field]]
addBoundary rows = fmap (\row -> EMPTY : row ++ [EMPTY]) augmentedRows
  where
    columnDim     = length $ head rows
    rowDim        = length rows
    emptyRow      = replicate columnDim EMPTY
    augmentedRows = emptyRow : rows ++ [emptyRow]

flipFields :: [[Field]] -> [[Field]]
flipFields rows = head rows : go rows ++ [last rows]
  where
    go (prev:act:next:rs) = (head act : go' prev act next ++ [last act]) : go (act:next:rs)
    go _ = []

    go' (p':p:p'':ps) (a':a:a'':as) (n':n:n'':ns) = 
        flipField [p',p,p'',a',a'',n',n,n''] a : go' (p:p'':ps) (a:a'':as) (n:n'':ns)
    go' _ _ _ = []

    flipField _ EMPTY = EMPTY
    flipField neighs CHAIR
      | count isNotPerson neighs == length neighs = PERSON
      | otherwise = CHAIR
    flipField neighs PERSON
      | count isPerson neighs < 4 = PERSON
      | otherwise = CHAIR

flipFields' :: M.Map (Int,Int) Field -> M.Map (Int,Int) Field
flipFields' fs = M.fromList xs'
  where
    xs  = M.assocs fs
    xs' = fmap (\((x,y),_) -> ((x,y),flipField' x y fs)) xs

flipField' :: Int -> Int -> M.Map (Int,Int) Field -> Field
flipField' r c fs =
    case field of
      EMPTY  -> EMPTY
      CHAIR  -> if personCount > 0
                then CHAIR
                else PERSON
      PERSON -> if personCount < 5
                then PERSON
                else CHAIR
  where
    field = fs M.! (r,c)
    dirs = [(-1,-1),(-1,0),(-1,1),(0,-1),(0,1),(1,-1),(1,0),(1,1)]
    personCount = count pred dirs

    pred (dx,dy) = hitsPerson dx dy fs r c

hitsPerson :: Int -> Int -> M.Map (Int,Int) Field -> Int -> Int -> Bool
hitsPerson dx dy fs x y = go (x+dx) (y+dy)
  where
    go !x' !y' =
      case M.lookup (x',y') fs of
        Nothing     -> False
        Just PERSON -> True
        Just CHAIR  -> False
        Just EMPTY  -> go (x'+dx) (y'+dy)

count :: (a -> Bool) -> [a] -> Int
count = go 0
  where
    go !acc pred [] = acc
    go !acc pred (x:xs)
      | pred x = go (acc+1) pred xs
      | otherwise = go acc pred xs

toMap :: [[a]] -> M.Map (Int,Int) a
toMap = M.fromList . join . zipWith (\i row -> zipWith (\j x -> ((i,j),x)) [0..] row) [0..]

firstRepeating :: Eq a => [a] -> a
firstRepeating (x:x':xs)
  | x == x' = x
  | otherwise = firstRepeating (x':xs)
firstRepeating _ = error ""

part_1 :: [[Field]] -> Int
part_1 fields = count isPerson . join . firstRepeating $ history
  where
    history = iterate flipFields . addBoundary $ fields

part_2 :: [[Field]] -> Int
part_2 fields = count isPerson . M.elems . firstRepeating $ history
  where
    map = toMap fields
    history = iterate flipFields' map
