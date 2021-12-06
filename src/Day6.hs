{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Day6 where

import FileAccess
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Map as M
import Data.List (iterate')
import Data.Maybe (fromJust)

type Timer = Integer
type FishesCount = Integer
type Fishes = M.Map Timer FishesCount

inputs = fmap (either (const 0) fst . TR.decimal) <$> (T.splitOn "," <$> readWholeFile "resources/Day6Input.txt")

initializeMap :: [Timer] -> Fishes
initializeMap = foldl addFish M.empty
  where addFish :: Fishes -> Timer -> Fishes
        addFish f t = M.alter (\case Just x -> Just (x+1)
                                     Nothing -> Just 1) t f

dayStep :: Fishes -> Fishes
dayStep f = foldl processTimer M.empty (M.keys f)
  where processTimer :: Fishes -> Timer -> Fishes
        processTimer f' 0 = let amount = fromJust $ M.lookup 0 f in M.insert 8 amount $ M.alter (\case Just x -> Just (x + amount)
                                                                                                       Nothing -> Just amount) 6 f'
        processTimer f' x = let amount = fromJust $ M.lookup x f in M.alter (\case Just x -> Just (x + amount)
                                                                                   Nothing -> Just amount) (x-1) f'
day6Task1 = fmap (processXDays 80) inputs

day6Task2 = fmap (processXDays 256) inputs

processXDays :: Int -> [Timer] -> Integer
processXDays i = sum . M.elems . (\x -> iterate' dayStep x !! i) . initializeMap