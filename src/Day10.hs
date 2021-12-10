module Day10 where

import Data.Either
import Data.List
import qualified Data.Text as T
import FileAccess
import Data.Maybe (fromJust, fromMaybe)

type ErrorChar = Char
type CharsToMatch = [Char]

inputs = fmap T.unpack <$> readLines "resources/Day10Input.txt"

legalOpeningChars = ['(', '[', '{', '<']

legalClosingChar c = fromJust $ lookup c [('(', ')'), ('[', ']'), ('{', '}'), ('<', '>')]

syntaxErrorPoints :: Char -> Integer
syntaxErrorPoints c = fromMaybe 0 $ lookup c [(')', 3), (']', 57), ('}', 1197), ('>', 25137)]

autocompletePoints :: Char -> Integer
autocompletePoints c = fromMaybe 0 $ lookup c [(')', 1), (']', 2), ('}', 3), ('>', 4)]

calculateAutocompletePoints :: String -> Integer
calculateAutocompletePoints = foldl (\acc c -> acc * 5 + autocompletePoints c) 0

-- processes input string, returning ErrorChar if given invalid closing char, or CharsToMatch
-- with collection of chars that need to be matched but EOF appeared
processChunks :: String -> Either ErrorChar CharsToMatch
processChunks = foldl processChar (Right [])
  where
    processChar :: Either ErrorChar CharsToMatch -> Char -> Either ErrorChar CharsToMatch
    processChar (Left c) _ = Left c -- Error is propagated
    processChar (Right []) c = if c `elem` legalOpeningChars then Right [c] else Left c
    processChar (Right (x : xs)) c
      | c `elem` legalOpeningChars = Right (c : x : xs) -- If c is opening, add to be matched
      | c == legalClosingChar x = Right xs -- If c is closing and matches opening, then trim head of to be matched
      | otherwise = Left c

day10Task1 = fmap ((sum . map syntaxErrorPoints . lefts) . fmap processChunks) inputs

day10Task2 = fmap (\l -> let results = getResults l in results !! (length results `div` 2)) inputs
  where
    getResults :: [String] -> [Integer]
    getResults = sort . (map (calculateAutocompletePoints . map legalClosingChar) . rights) . map processChunks