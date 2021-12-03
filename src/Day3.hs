{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Day3 where

import FileAccess
import Data.Bits
import Numeric
import qualified Data.Text as T
import GHC.Natural
import Data.Maybe (fromMaybe)

inputs = (fmap . fmap) (parseBin . T.unpack) (readLines "resources/Day3Input.txt")

inputBitLength = 12
wordBitLength = fromMaybe 64 $ bitSizeMaybe (zeroBits :: Word)
lengthDifference = wordBitLength - inputBitLength

parseBin :: String -> Word = fromInteger . fst . head . readInt 2 (const True) (\c -> if c == '0' then 0 else 1)
inputBitsCount = 12
wordBitsCount = fromMaybe 64 $ bitSizeMaybe (zeroBits :: Word)

determineOGR :: [Word] -> Int -> Word 
determineOGR [x] _ = x
determineOGR xs i = case compare (length onesAtI) (length zeroesAtI) of
  EQ -> determineOGR onesAtI (i - 1)
  LT -> determineOGR zeroesAtI (i - 1)
  GT -> determineOGR onesAtI (i - 1)
  where onesAtI = filter (`testBit` i) xs
        zeroesAtI = filter (\x -> not (testBit x i)) xs

determineCO2 :: [Word] -> Int -> Word
determineCO2 [x] _ = x
determineCO2 xs i = case compare (length onesAtI) (length zeroesAtI) of
  EQ -> determineCO2 zeroesAtI (i - 1)
  LT -> determineCO2 onesAtI (i - 1)
  GT -> determineCO2 zeroesAtI (i - 1)
  where onesAtI = filter (`testBit` i) xs
        zeroesAtI = filter (\x -> not (testBit x i)) xs

determineBits :: [Word] -> Int -> String
determineBits l (-1) = ""
determineBits l i = determineBit l i : determineBits l (i-1)
  where determineBit l i = let bits = map (`testBit` i) l in if length (filter id bits) >= length (filter not bits) then '1' else '0'

bitNegate :: Word -> Word
bitNegate = (`shift` (- lengthDifference)) . (`shift` lengthDifference) . complement

day3Task1 = fmap processInputs inputs
  where processInputs :: [Word] -> Word
        processInputs x = let res = parseBin $ determineBits x inputBitLength in res * bitNegate res

day3Task2 = fmap processInputs inputs
  where processInputs :: [Word] -> Word
        processInputs x = let 
          (ogr, co2) = (determineOGR x (inputBitLength - 1), determineCO2 x (inputBitLength - 1)) 
          in ogr * co2