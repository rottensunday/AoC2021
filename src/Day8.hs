{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Day8 where

import FileAccess
import qualified Data.Text as T
import Text.Megaparsec
import Data.Void (Void)
import Text.Megaparsec.Char (alphaNumChar, space, lowerChar, string, letterChar)
import Control.Monad
import Data.Either (fromRight)
import Data.Maybe (fromJust)
import qualified Data.Map as M
import Data.List

type Parser = Parsec Void T.Text

type SignalPattern = [Integer]
type OutputDigit = [Integer]
data Entry = Entry { signalPatterns :: [SignalPattern]
                   , outputDigits   :: [OutputDigit]
                   } deriving Show

charToInt c = fromJust $ lookup c [('a',0),('b',1),('c',2),('d',3),('e',4),('f',5),('g',6)]

elemsContaining = [ ([0,1,2,4,5,6], 0)
                  , ([2,5], 1)
                  , ([0,2,3,4,6], 2)
                  , ([0,2,3,5,6], 3)
                  , ([1,2,3,5], 4)
                  , ([0,1,3,5,6], 5)
                  , ([0,1,3,4,5,6], 6)
                  , ([0,2,5], 7)
                  , ([0,1,2,3,4,5,6], 8)
                  , ([0,1,2,3,5,6], 9)
                  ]

inputs = fmap parseLine <$> readLines "resources/Day8Input.txt"

entryParser :: Parser Entry
entryParser = do
  signalPatterns <- some (sort <$> some (charToInt <$> letterChar) <* optional space)
  void (string "| ")
  outputDigits <- some (sort <$> some (charToInt <$> letterChar) <* optional space)
  return Entry {..}

parseLine :: T.Text -> Entry
parseLine t = fromRight (Entry [] []) (parse entryParser "" t)

day8Task1 = fmap (sum . fmap (length . filter acceptableLength . outputDigits)) inputs
  where acceptableLength x = length x `elem` [2,4,3,7]

encodings = permutations [0..6]

checkPermutation :: [SignalPattern] -> [Integer] -> Bool
checkPermutation [] _ = True
checkPermutation (x:xs) is = case lookup (sort $ map (\y -> fromJust $ y `elemIndex` is) x) elemsContaining of
                                  Nothing -> False
                                  Just x -> checkPermutation xs is

fromPermutation :: [Integer] -> SignalPattern -> Integer
fromPermutation is p = fromJust $ lookup (sort $ map (\y -> fromJust $ y `elemIndex` is) p) elemsContaining

findPermutation :: [[Integer]] -> [Integer]
findPermutation sp = head $ filter (checkPermutation sp) encodings

day8Task2 = fmap (sum . fmap processEntry) inputs
  where processEntry :: Entry -> Integer
        processEntry (Entry patterns outputs) =
          let permutation = findPermutation patterns in
            read $ concatMap (show . fromPermutation permutation) outputs


-- checkPermutation [] [] = error "coo"

-- [SignalPattern] -> [(Int, [[Char]])]

-- intersections :: M.Map Integer [[Char]] -> M.Map Integer [Char]
-- intersections = M.map concatIntersection

-- processPatterns :: [SignalPattern] -> M.Map Integer [[Char]]
-- processPatterns = foldr foldFn M.empty
--   where
--     foldFn :: SignalPattern -> M.Map Integer [[Char]] -> M.Map Integer [[Char]]
--     foldFn sp acc = let processResult = processSignalPatternInput sp
--                       in foldr innerFoldFn acc processResult
--     innerFoldFn :: (Integer, [Char]) -> M.Map Integer [[Char]] -> M.Map Integer [[Char]]
--     innerFoldFn ic@(i, chars) acc = M.alter (\case Nothing -> Just [chars]
--                                                    Just x -> Just $ chars:x) i acc

-- processSignalPatternInput :: SignalPattern -> [(Integer, [Char])]
-- processSignalPatternInput p = fmap buildLookup nums
--   where nums = fromJust $ lookup (length p) nElems
--         buildLookup :: Integer -> (Integer, [Char])
--         buildLookup x = (x, p)

-- concatIntersection :: Eq a => [[a]] -> [a]
-- concatIntersection = foldl1 intersect