{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}

module A14 (
             input
           , part_1
           , part_2
           , result_1
           , result_2
           , parseInput
           ) where

import qualified Text.Parsec            as P
import qualified Data.IntMap            as I
import           Data.List     (foldl')
import           Control.Monad (join)

input :: IO String
input = readFile "input/a14_1.txt"

result_1 :: IO Int
result_1 = part_1 . parseInput <$> input

result_2 :: IO Int
result_2 = part_2 . parseInput <$> input

data OP = Mask [(Int,Bit)]
        | Mem Int Int
      deriving Show

data Bit = Low
         | High
       deriving Show

data ProgState = ProgState
                   {
                     unMask :: [(Int,Bit)]
                   , unMem  :: I.IntMap Int
                   }
            deriving Show

bitParser :: P.Parsec String () (Maybe Bit)
bitParser = do
  c <- P.oneOf "X01"
  case c of
    'X' -> return Nothing
    '0' -> return $ Just Low
    '1' -> return $ Just High

bitsToMask :: [Maybe Bit] -> OP
bitsToMask = Mask . go 0
  where
    go !c []     = []
    go !c (b:bs) =
      case b of
        Nothing -> go (c+1) bs
        Just b' -> (c,b') : go (c+1) bs

maskParser :: P.Parsec String () OP
maskParser = do
  _    <- P.string "mask = "
  bits <- P.count 36 bitParser
  return $ bitsToMask bits

memParser :: P.Parsec String () OP
memParser = do
  _       <- P.string "mem["
  address <- P.many1 P.digit
  _       <- P.string "] = "
  value   <- P.many1 P.digit
  return $ Mem (read address) (read value)

opParser :: P.Parsec String () OP
opParser = P.try maskParser P.<|> memParser

parseInput :: String -> [OP]
parseInput = flip (.) (P.parse (P.endBy opParser (P.char '\n')) "input") $
               \case
                 Left err  -> error $ show err
                 Right ops -> ops

eval :: ProgState -> OP -> ProgState
eval (ProgState mask mem) (Mask mask')   = ProgState mask' mem
eval (ProgState mask mem) (Mem addr val) = ProgState mask mem'
  where
    val' = applyMask mask val
    mem' = I.insert addr val' mem

applyMask :: [(Int,Bit)] -> Int -> Int
applyMask mask = bitsToInt . applyMask' mask . intToBits

applyMask' :: [(Int,Bit)] -> [Bit] -> [Bit]
applyMask' mask = go mask 0
  where
    go [] _ bs                   = bs
    go ((!i,!m):ms) !j ((!b):bs) =
      case i `compare` j of
        LT -> error "applying mask"
        EQ -> m : go ms (j+1) bs
        GT -> b : go ((i,m):ms) (j+1) bs
    go _ _ _ = error "applying mask"

intToBit :: Int -> Bit
intToBit 0 = Low
intToBit _ = High

intToBits :: Int -> [Bit]
intToBits = reverse . take 36 . (++ repeat Low) . go
  where
    go 0  = []
    go !i = let (!q,!r) = i `quotRem` 2
            in  intToBit r : go q

bitsToInt :: [Bit] -> Int
bitsToInt = go 0
  where
    go !acc []     = acc
    go !acc (b:bs) = case b of
                       Low  -> go (2*acc) bs
                       High -> go (2*acc + 1) bs

maskAddress :: [(Int,Bit)] -> Int -> [Int]
maskAddress mask = fmap bitsToInt . maskAddress' mask . intToBits

maskAddress' :: [(Int,Bit)] -> [Bit] -> [[Bit]]
maskAddress' mask = go mask 0
  where
    go [] _ []                 = [[]]
    go [] !j ((!b):bs)         =
      let bss = go [] (j+1) bs
      in  do
           bit  <- [Low,High]
           addr <- bss
           return $ bit : addr
    go ((!i,!m):ms) !j ((!b):bs) =
      case i `compare` j of
        LT -> error "applying mask"
        EQ -> case m of
                Low  -> (b:)    <$> go ms (j+1) bs
                High -> (High:) <$> go ms (j+1) bs
        GT -> let bss = go ((i,m):ms) (j+1) bs
              in do
                    bit  <- [Low,High]
                    addr <- bss
                    return $ bit : addr
    go _ _ _ = error "applying mask"

eval' :: ProgState -> OP -> ProgState
eval' (ProgState mask mem) (Mask mask')   = ProgState mask' mem
eval' (ProgState mask mem) (Mem addr val) = ProgState mask mem'
  where
    addresses = maskAddress mask addr
    mem' = foldl' (\m a -> I.insert a val m) mem addresses

part_1 :: [OP] -> Int
part_1 = I.foldl' (+) 0 .
         unMem .
         foldl' eval (ProgState [] I.empty)

part_2 :: [OP] -> Int
part_2 = I.foldl' (+) 0 .
         unMem .
         foldl' eval' (ProgState [] I.empty)
