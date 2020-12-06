{-# LANGUAGE TupleSections #-}

module A04 (
             input
           , part_1
           , part_2
           , result_1
           , result_2
           ) where

import qualified Text.Parsec as P
import Data.List (sort)
import Data.Maybe (catMaybes, isJust)
import Control.Applicative (liftA2)

input :: IO String
input = readFile "input/a04_1.txt"

type Key = String

data Val = Byr Int
         | Iyr Int
         | Eyr Int
         | Hgt HInt
         | Hcl Int
         | Ecl EyeCol
         | Pid Int
         | Cid String
       deriving Show

data HInt = Centimeters Int
          | Inches Int
        deriving Show

data EyeCol = AMB
            | BLU
            | BRN
            | GRY
            | GRN
            | HZL
            | OTH
          deriving Show

type Passport = [(Key, Maybe Val)]

eyeCol :: String -> Maybe EyeCol
eyeCol "amb" = Just AMB
eyeCol "blu" = Just BLU
eyeCol "brn" = Just BRN
eyeCol "gry" = Just GRY
eyeCol "grn" = Just GRN
eyeCol "hzl" = Just HZL
eyeCol "oth" = Just OTH
eyeCol _     = Nothing

byrParser :: P.Parsec String st Val
byrParser = do
  year <- read <$> P.count 4 P.digit
  P.eof
  if year < 1920 || year > 2002
    then fail "invalid year"
    else return $ Byr year

iyrParser :: P.Parsec String st Val
iyrParser = do
  year <- read <$> P.count 4 P.digit
  P.eof
  if year < 2010 || year > 2020
    then fail "invalid year"
    else return $ Iyr year

eyrParser :: P.Parsec String st Val
eyrParser = do
  year <- read <$> P.count 4 P.digit
  P.eof
  if year < 2020 || year > 2030
    then fail "invalid year"
    else return $ Eyr year

hgtParser :: P.Parsec String st Val
hgtParser = do
  hgt <- P.try centimetersParser P.<|> inchesParser
  P.eof
  return $ Hgt hgt

centimetersParser :: P.Parsec String st HInt
centimetersParser = do
  cms <- read <$> P.count 3 P.digit
  _   <- P.string "cm"
  if cms < 150 || cms > 193
    then fail "invalid height in cm"
    else return $ Centimeters cms

inchesParser :: P.Parsec String st HInt
inchesParser = do
  ins <- read <$> P.count 2 P.digit
  _   <- P.string "in"
  if ins < 59 || ins > 76
    then fail "invalid height in inches"
    else return $ Inches ins

hclParser :: P.Parsec String st Val
hclParser = do
  _ <- P.char '#'
  hex <- P.count 6 P.hexDigit
  P.eof
  return $ Hcl $ read $ "0x" ++ hex

eclParser :: P.Parsec String st Val
eclParser = do
  code <- P.count 3 P.lower
  P.eof
  case eyeCol code of
    Just col -> return $ Ecl col
    _        -> fail "invalid eye color"

pidParser :: P.Parsec String st Val
pidParser = do
  pid <- read <$> P.count 9 P.digit
  P.eof
  return $ Pid pid

cidParser :: P.Parsec String st Val
cidParser = do
  cid <- P.many1 $ P.noneOf " \n"
  P.eof
  return $ Cid cid

valParser :: P.Parsec String () Val -> P.Parsec String st (Maybe Val)
valParser valParser = do
  val <- P.many1 $ P.noneOf " \n"
  case P.parse valParser "" val of
    Left e -> return Nothing
    Right v -> return $ Just v

keyValParser :: P.Parsec String st (Key, Maybe Val)
keyValParser = do
  key <- P.count 3 P.lower P.<?> "key"
  case key of
    "byr" -> P.char ':' >> ("byr",) <$> valParser byrParser
    "iyr" -> P.char ':' >> ("iyr",) <$> valParser iyrParser
    "eyr" -> P.char ':' >> ("eyr",) <$> valParser eyrParser
    "hgt" -> P.char ':' >> ("hgt",) <$> valParser hgtParser
    "hcl" -> P.char ':' >> ("hcl",) <$> valParser hclParser
    "ecl" -> P.char ':' >> ("ecl",) <$> valParser eclParser
    "pid" -> P.char ':' >> ("pid",) <$> valParser pidParser
    "cid" -> P.char ':' >> ("cid",) <$> valParser cidParser
    _ -> fail "invalid key value pair"

passportParser :: P.Parsec String st Passport
passportParser = P.many1 $ do
  kv <- keyValParser
  _ <- P.oneOf " \n"
  return kv

passportsParser :: P.Parsec String st [Passport]
passportsParser = P.sepBy1 (P.try passportParser) (P.char '\n')

mandatory :: [String]
mandatory = sort [
                   "byr"
                 , "iyr"
                 , "eyr"
                 , "hgt"
                 , "hcl"
                 , "ecl"
                 , "pid"
                 ]

readPassports :: String -> [Passport]
readPassports inputString =
  case P.parse passportsParser "passports" inputString of
    Left e          -> error (show e)
    Right passports -> passports

isPassportComplete :: Passport -> Bool
isPassportComplete ps = keys == mandatory
  where
    keys = sort $ filter (/= "cid") $ fmap fst ps

isPassportValid :: Passport -> Bool
isPassportValid = all (isJust . snd)

result_1 :: IO Int
result_1 = do
  passports <- readPassports <$> input
  return $ part_1 passports

result_2 :: IO Int
result_2 = do
  passports <- readPassports <$> input
  return $ part_2 passports

part_1 :: [Passport] -> Int
part_1 passports = length $ filter isPassportComplete passports

part_2 :: [Passport] -> Int
part_2 passports = length completeAndValidPassports
  where
    completeAndValidPassports =
      filter (liftA2 (&&) isPassportComplete isPassportValid) passports
