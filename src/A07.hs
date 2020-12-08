{-# LANGUAGE TupleSections #-}

module A07
  ( input
  , part_1
  , part_2
  , result_1
  , result_2
  ) where

import qualified Data.Set               as S
import qualified Data.Map               as M
import qualified Data.IntMap            as I
import qualified Text.Parsec            as P
import           Data.List   ( sort
                             , nub
                             , foldl' )

input :: IO String
input = readFile "input/a07_1.txt"

type Color = String

bagParser :: P.Parsec String st Color
bagParser = do
  word1 <- P.many1 P.letter
  _     <- P.char ' '
  word2 <- P.many1 P.letter
  _     <- P.char ' '
  _     <- P.string "bag"
  _     <- P.optional $ P.char 's'
  return $ word1 ++ " " ++ word2

bagWithNumberParser :: P.Parsec String st (Int, Color)
bagWithNumberParser = do
  number <- P.many1 P.digit
  _      <- P.char ' '
  bag    <- bagParser
  return (read number, bag)

innerParser :: P.Parsec String st [(Int, Color)]
innerParser = (P.string "no other bags" >> return []) P.<|>
              P.sepBy1 bagWithNumberParser (P.string ", ")

lineParser :: P.Parsec String st (Color, [(Int,Color)])
lineParser = do
  outer <- bagParser
  _     <- P.string " contain "
  inner <- innerParser
  _     <- P.string ".\n"
  return (outer, inner)

parseInput :: String -> [(Color, [(Int,Color)])]
parseInput str = case P.parse (P.many1 lineParser) "input" str of
                   Left e      -> error $ show e
                   Right edges -> edges

distribute :: (a,[b]) -> [(a,b)]
distribute (x,ys) = fmap (x,) ys

type Graph a = S.Set (a,a)

vertices :: Ord a => Graph a -> [a]
vertices = nub . sort . concatMap (\(a,b) -> [a,b]) . S.elems

relabel :: Ord a => M.Map a Int -> Graph a -> Graph Int
relabel replaceMap = S.map (\(a,b) -> (replaceMap M.! a, replaceMap M.! b))

removeNumbers :: (a,[(b,c)]) -> (a,[c])
removeNumbers (x,ys) = (x, fmap snd ys)

inputToGraph :: Ord a => [(a, [(b,a)])] -> Graph a
inputToGraph = S.fromList . concatMap (distribute . removeNumbers)

transitiveClosure :: Int -> Graph Int -> Graph Int
transitiveClosure n g =
    foldl' (\acc (i,j) -> if (j,i) `S.member` acc
                          then foldl' (\acc' k -> if (i,k) `S.member` acc'
                                                  then (j,k) `S.insert` acc'
                                                  else acc') acc vs
                          else acc) g $ (,) <$> vs <*> vs
  where
    vs = take n [0..]

type Graph' = I.IntMap [(Int,Int)]

data Tree a = Node a [Tree a] deriving Show

unfoldFromGraph' :: Graph' -> Int -> [Tree Int]
unfoldFromGraph' g n = fmap (\(w,n') -> Node w $ unfoldFromGraph' g n') bs
  where
    bs = I.findWithDefault [] n g

reduce :: Tree Int -> Int
reduce (Node n ts) = n * (1 + sum (fmap reduce ts))

part_1 :: [(Color, [(Int,Color)])] -> Int
part_1 statements =
    length $ filter ((== shinyGold) . snd) $ S.elems closure
  where
    g          = inputToGraph statements
    vs         = vertices g
    n          = length vs
    relabelMap = M.fromList $ zip vs [0..]
    shinyGold  = relabelMap M.! "shiny gold"
    g'         = relabel relabelMap g
    closure    = transitiveClosure n g'

part_2 :: [(Color, [(Int,Color)])] -> Int
part_2 statements = reduce t
  where
    edges      = nub $ sort $ concatMap distribute statements
    vs         = nub $ sort $ concatMap (\(a,(_,b)) -> [a,b]) edges
    relabelMap = M.fromList $ zip vs [0..]
    shinyGold  = relabelMap M.! "shiny gold"
    g          = I.fromList $
                 fmap (\(a,bs) -> ( relabelMap M.! a
                                  , sort $ fmap (\(w,b) -> (w, relabelMap M.! b)) bs
                                  )) statements
    t          = Node 1 $ unfoldFromGraph' g shinyGold

result_1 :: IO Int
result_1 = part_1 . parseInput <$> input

result_2 :: IO Int
result_2 = part_2 . parseInput <$> input
