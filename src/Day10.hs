module Day10 where

import Data.Either
import Data.List
import qualified Data.Text as T
import FileAccess
import Data.Maybe (fromJust, fromMaybe)

inputs = fmap T.unpack <$> readLines "resources/Day10Input.txt"

legalOpeningChars = ['(', '[', '{', '<']

legalClosingChar c = fromJust $ lookup c [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

syntaxErrorPoints :: Char -> Integer
syntaxErrorPoints c = fromMaybe 0 $ lookup c [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

autocompletePoints :: Char -> Integer
autocompletePoints c = fromMaybe 0 $ lookup c [(')', 1), (']', 2), ('}', 3), ('>', 4)]

calculateAutocompletePoints :: String -> Integer
calculateAutocompletePoints = foldl (\acc c -> acc * 5 + autocompletePoints c) 0

closingChars :: String -> Either Char String
closingChars = foldl processChar (Right [])
  where
    processChar :: Either Char String -> Char -> Either Char String
    processChar (Left c) _ = Left c
    processChar (Right []) c = if c `elem` legalOpeningChars then Right [c] else Left c
    processChar (Right (x : xs)) c
      | c `elem` legalOpeningChars = Right (c : x : xs)
      | c == legalClosingChar x = Right xs
      | otherwise = Left c

day10Task1 = fmap ((sum . map syntaxErrorPoints . lefts) . fmap closingChars) inputs

day10Task2 = fmap (\l -> let results = getResults l in results !! (length results `div` 2)) inputs
  where
    getResults :: [String] -> [Integer]
    getResults = sort . (map (calculateAutocompletePoints . map legalClosingChar) . rights) . map closingChars