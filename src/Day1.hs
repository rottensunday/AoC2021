module Day1 where

import FileAccess
import qualified Data.Text as T

inputs = (fmap . fmap) ((read :: String -> Int) . T.unpack) (readLines "resources/Day1Input.txt")

increasesCount :: [Int] -> Int
increasesCount [] = 0
increasesCount (x:y:xs) = if y > x then 1 + increasesCount (y:xs) else increasesCount (y:xs)
increasesCount (x:_) = 0

threeSums :: [Int] -> [Int]
threeSums x = zipWith3 (\x y z -> x + y + z) x (drop 1 x) (drop 2 x)

day1Task1 = fmap increasesCount inputs
day1Task2 = fmap (increasesCount . threeSums) inputs