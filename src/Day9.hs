module Day9 where

import Data.Char (digitToInt)
import Data.List
import Data.Maybe
import qualified Data.Text as T
import FileAccess

inputs = fmap (map digitToInt . T.unpack) <$> readLines "resources/Day9Input.txt"

(!?) :: [a] -> Int -> Maybe a
(!?) l i
  | i < 0 || i >= length l = Nothing
  | otherwise = Just $ l !! i

(!!!) :: [[a]] -> (Int, Int) -> Maybe a
(!!!) l (x, y) = (!? y) =<< (l !? x)

neighborhood :: [[Int]] -> (Int, Int) -> [Int]
neighborhood l (x, y) = catMaybes [l !!! (x -1, y), l !!! (x + 1, y), l !!! (x, y -1), l !!! (x, y + 1)]

minimumValues :: [[Int]] -> [(Int, (Int, Int))]
minimumValues l = filter checkMinimum $ concat lWithCoords
  where
    lWithCoords = zipWith (\l' i -> zipWith (\x j -> (x, (i, j))) l' [0 ..]) l [0 ..]
    checkMinimum :: (Int, (Int, Int)) -> Bool
    checkMinimum (i, coord) =
      let neighb = neighborhood l coord
       in notElem i neighb && minimum (i : neighb) == i

getBasinElems :: [[Int]] -> (Int, Int) -> [Int]
getBasinElems l (x, y) = map (\i -> fromJust $ l !!! i) $ go [] (x, y)
  where
    go :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
    go processed (x', y') =
      if (x', y') `elem` processed
        then processed
        else case l !!! (x', y') of
          Nothing -> processed
          Just 9 -> processed
          Just x -> go (go (go (go ((x', y') : processed) (x' -1, y')) (x' + 1, y')) (x', y' -1)) (x', y' + 1)

day9Task1 = fmap (sum . map ((+ 1) . fst) . minimumValues) inputs

day9Task2 = fmap (\l -> product $ take 3 $ sortBy (flip compare) $ map (length . (getBasinElems l . snd)) (minimumValues l)) inputs