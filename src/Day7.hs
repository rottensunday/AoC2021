{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day7 where

import FileAccess
import qualified Data.Text as T
import qualified Data.Text.Read as TR

type CalculateCostFunction = Int -> Int -> Int

inputs = fmap (either (const 0) fst . TR.decimal) <$> (T.splitOn "," <$> readWholeFile "resources/Day7Input.txt")

changeValueCost :: [Int] -> CalculateCostFunction -> Int -> Int
changeValueCost l f i = foldl (\acc x -> f x i + acc) 0 l

task1CalculationFn x y = abs (x-y)

task2CalculationFn x y = let difference = abs (x-y) in difference * (difference + 1) `div` 2

solve f = fmap (\x -> minimum $ map (changeValueCost x f) [minimum x..maximum x]) inputs

day7Task1 = solve task1CalculationFn

day7Task2 = solve task2CalculationFn