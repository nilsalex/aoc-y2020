module Main where

import qualified A01
import qualified A02
import qualified A03
import qualified A04
import qualified A05
import qualified A06
import qualified A07
import qualified A08

main :: IO ()
main = do
  putStrLn "Day 1"
  print =<< A01.result_1
  print =<< A01.result_2
  putStrLn "\nDay 2"
  print =<< A02.result_1
  print =<< A02.result_2
  putStrLn "\nDay 3"
  print =<< A03.result_1
  print =<< A03.result_2
  putStrLn "\nDay 4"
  print =<< A04.result_1
  print =<< A04.result_2
  putStrLn "\nDay 5"
  print =<< A05.result_1
  print =<< A05.result_2
  putStrLn "\nDay 6"
  print =<< A06.result_1
  print =<< A06.result_2
  putStrLn "\nDay 7"
  print =<< A07.result_1
  print =<< A07.result_2
  putStrLn "\nDay 8"
  print =<< A08.result_1
  print =<< A08.result_2
