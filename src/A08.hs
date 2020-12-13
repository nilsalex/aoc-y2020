module A08 ( input
           , part_1
           , part_2
           , result_1
           , result_2
           ) where

import qualified Text.Parsec         as P
import qualified Data.Set            as S

input :: IO String
input = readFile "input/a08_1.txt"

result_1 :: IO Int
result_1 = part_1 . parseProgram <$> input

result_2 :: IO Int
result_2 = part_2 . parseProgram <$> input

data OP = ACC Int
        | JMP Int
        | NOP Int
        | HLT
      deriving Show

data Program = Program
     { accumulator :: Int
     , pointer     :: Int
     , prevStack   :: [OP]
     , pointerOp   :: OP
     , nextStack   :: [OP]
     } deriving Show

opParser :: P.Parsec String st OP
opParser = do
  op    <- P.count 3 $ P.oneOf "accjmnop"
  _     <- P.char ' '
  _     <- P.option "" $ P.string "+"
  minus <- P.option "" $ P.string "-"
  num   <- P.many1 P.digit
  _     <- P.char '\n'
  case op of
    "acc" -> return $ ACC $ read (minus ++ num)
    "jmp" -> return $ JMP $ read (minus ++ num)
    "nop" -> return $ NOP $ read (minus ++ num)

programParser :: P.Parsec String st [OP]
programParser = P.many1 opParser

parseProgram :: String -> Program
parseProgram i = case P.parse programParser "input" i of
                   Left e       -> error $ show e
                   Right []     -> error "empty program"
                   Right (o:os) -> Program 0 0 [] o (os ++ [HLT])

eval :: Program -> Either Program Program
eval (Program acc pos ps op ns) =
  case op of
    HLT   -> Left (Program acc pos ps op ns)
    ACC x -> case ns of
               []    -> error "empty next stack"
               n:ns' -> Right $ Program (acc+x) (pos+1) (op:ps) n ns'
    JMP x -> case jump x ps op ns of
               Nothing            -> error "cannot jump before program"
               Just (ps',op',ns') -> Right $ Program acc (pos+x) ps' op' ns'
    NOP _ -> case ns of
               []    -> error "empty next stack"
               n:ns' -> Right $ Program acc (pos+1) (op:ps) n ns'

jump :: Int -> [OP] -> OP -> [OP] -> Maybe ([OP],OP,[OP])
jump x ps op ns =
  case x `compare` 0 of
    LT -> case ps of
            []    -> Nothing
            p:ps' -> jump (x+1) ps' p (op:ns)
    EQ -> Just (ps,op,ns)
    GT -> case op of
            HLT -> Just (ps,op,ns)
            _   ->
              case ns of
                []         -> error "empty next stack"
                n:ns'      -> jump (x-1) (op:ps) n ns'

unfold :: (a -> Either a a) -> a -> [a]
unfold f x = case f x of
               Left x'  -> [x, x']
               Right x' -> x : unfold f x'

buildHistory :: Program -> [(Int, Int)]
buildHistory = fmap (\(Program acc p _ _ _) -> (acc, p)) . unfold eval

patchAll :: (a -> Maybe a) -> [a] -> [[a]]
patchAll _     []         = []
patchAll patch xs@(x:xs') =
  case patch x of
    Nothing -> fmap (x:) (patchAll patch xs')
    Just x' -> (x':xs') : fmap (x:) (patchAll patch xs')

unfold' :: (a -> Maybe a) -> (a -> Either a a) -> a -> [[a]]
unfold' modify f x = case modify x of
                        Nothing -> xs
                        Just mx -> unfold f mx : xs
  where
    xs = case f x of
           Left x'  -> [[x,x']]
           Right x' -> fmap (x :) (unfold' modify f x')

findCycle :: (a -> a -> Bool) -> [a] -> ([a],[a])
findCycle eqCompare xxs = fCycle xxs xxs
  where fCycle (x:xs) (_:y:ys)
         | x `eqCompare` y              = fStart xxs xs
         | otherwise           = fCycle xs ys
        fCycle _      _        = (xxs,[]) -- not cyclic
        fStart (x:xs) (y:ys)
         | x `eqCompare` y              = ([], x:fLength x xs)
         | otherwise           = let (as,bs) = fStart xs ys in (x:as,bs)
        fLength x (y:ys)
         | x `eqCompare` y              = []
         | otherwise           = y:fLength x ys

patchOp :: OP -> Maybe OP
patchOp (JMP x) = Just $ NOP x
patchOp (NOP x) = Just $ JMP x
patchOp _       = Nothing

part_1 :: Program -> Int
part_1 = fst . last . snd . findCycle eqCompare . buildHistory
  where
    eqCompare (_,ptr) (_,ptr') = ptr == ptr'

part_2 :: Program -> Int
part_2 prog = fst . last . fst . head . filter (null . snd) . fmap (findCycle eqCompare) $ hs
  where
    stack  = pointerOp prog : nextStack prog
    stacks = patchAll patchOp stack
    progs  = fmap (\(op:ops) -> Program 0 0 [] op ops) stacks
    hs = fmap buildHistory progs
    eqCompare (_,ptr) (_,ptr') = ptr == ptr'
