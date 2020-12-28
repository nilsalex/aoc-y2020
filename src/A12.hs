module A12 ( input
           , part_1
           , part_2
           , result_1
           , result_2
           ) where

input :: IO String
input = readFile "input/a12_1.txt"

result_1 :: IO Int
result_1 = part_1 . parseInput <$> input

result_2 :: IO Int
result_2 = part_2 . parseInput <$> input

data OP = N Int
        | S Int
        | W Int
        | E Int
        | L Int
        | R Int
        | F Int
      deriving Show

fromChar :: Char -> Int -> OP
fromChar 'N' = N
fromChar 'S' = S
fromChar 'W' = W
fromChar 'E' = E
fromChar 'L' = L
fromChar 'R' = R
fromChar 'F' = F
fromChar c   = error $ "unknown no OP defined for code " <> [c]

parseOp :: String -> OP
parseOp (x:xs) = fromChar x (read xs)
parseOp _      = error "empty line"

parseInput :: String -> [OP]
parseInput = fmap parseOp . lines

data ProgState = ProgState ![OP] !Dir !Pos deriving Show
data Dir = Dir !Int !Int deriving Show
data Pos = Pos !Int !Int deriving Show
data WPos = WPos !Int !Int deriving Show
data ProgState' = ProgState' ![OP] !WPos !Pos deriving Show

eval :: ProgState -> ProgState
eval s@(ProgState [] _ _) = s
eval (ProgState (o:os) dir@(Dir dx dy) pos@(Pos x y)) =
    case o of
      N n -> ProgState os dir (Pos x (y+n))
      S n -> ProgState os dir (Pos x (y-n))
      W n -> ProgState os dir (Pos (x-n) y)
      E n -> ProgState os dir (Pos (x+n) y)
      L d -> ProgState os (turn d dir) pos
      R d -> let d' = 360 - d
             in ProgState os (turn d' dir) pos
      F n -> ProgState os dir (Pos (x+n*dx) (y+n*dy))
  where
    turn d (Dir dx' dy') =
        case d of
          0   -> Dir dx' dy'
          90  -> Dir (-dy') dx'
          180 -> Dir (-dx') (-dy')
          270 -> Dir dy' (-dx')
          _   -> error $ "rotation around " <> show d <> " degrees not implemented"
      where
        d' = d `mod` 360

evalAll :: ProgState -> ProgState
evalAll s@(ProgState [] _ _) = s
evalAll s = evalAll . eval $ s

eval' :: ProgState' -> ProgState'
eval' s@(ProgState' [] _ _) = s
eval' (ProgState' (o:os) wpos@(WPos wx wy) pos@(Pos x y)) =
    case o of
      N n -> ProgState' os (WPos wx (wy+n)) pos
      S n -> ProgState' os (WPos wx (wy-n)) pos
      W n -> ProgState' os (WPos (wx-n) wy) pos
      E n -> ProgState' os (WPos (wx+n) wy) pos
      L d -> ProgState' os (turn d wpos) pos
      R d -> let d' = 360 - d
             in ProgState' os (turn d' wpos) pos
      F n -> ProgState' os wpos (Pos (x+n*wx) (y+n*wy))
  where
    turn d (WPos wx' wy') =
        case d of
          0   -> WPos wx' wy'
          90  -> WPos (-wy') wx'
          180 -> WPos (-wx') (-wy')
          270 -> WPos wy' (-wx')
          _   -> error $ "rotation around " <> show d <> " degrees not implemented"
      where
        d' = d `mod` 360

evalAll' :: ProgState' -> ProgState'
evalAll' s@(ProgState' [] _ _) = s
evalAll' s = evalAll' . eval' $ s

part_1 :: [OP] -> Int
part_1 os = abs x + abs y
  where
    ProgState _ _ (Pos x y) = evalAll $ ProgState os (Dir 1 0) (Pos 0 0)

part_2 :: [OP] -> Int
part_2 os = abs x + abs y
  where
    ProgState' _ _ (Pos x y) = evalAll' $ ProgState' os (WPos 10 1) (Pos 0 0)
