{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Day5 where

import FileAccess
import Text.Megaparsec
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import qualified Data.Text as T
import Text.Megaparsec.Char (char, string)
import Control.Monad
import Data.Either (fromRight)
import qualified Data.Map as M

type Coordinate = (Int, Int)
type Line = (Coordinate, Coordinate)
type Occurrences = Int
type Board = M.Map Coordinate Occurrences

type Parser = Parsec Void T.Text

inputs = fmap parseLine <$> readLines "resources/Day5Input.txt"

lineParser :: Parser Line
lineParser = do
  xs:ys:_ <- do replicateM 2 $ 
                  (,) 
                  <$> (L.decimal <* char ',') 
                  <*> (L.decimal <* many (string " -> "))
  return (xs, ys)

parseLine :: T.Text -> Line
parseLine t = fromRight ((0,0), (0,0)) (parse lineParser "" t)

linePoints :: Line -> [Coordinate]
linePoints ((x1, y1), (x2, y2))
  | x1 == x2 = fmap (x1,) (makeLine y1 y2)
  | y1 == y2 = fmap (,y1) (makeLine x1 x2)
  | otherwise = zip (makeLine x1 x2) (makeLine y1 y2)
  where makeLine x y
          | x >= y = reverse [y..x]
          | otherwise = [x..y]

foldLines :: [Line] -> Board
foldLines = foldl processLine M.empty
  where processLine :: Board -> Line -> Board
        processLine b l = foldl processCoord b $ linePoints l
        processCoord :: Board -> Coordinate -> Board
        processCoord b coord = M.alter (\case Just x -> Just (x+1)
                                              Nothing -> Just 1) coord b

straightLines :: [Line] -> [Line]
straightLines = filter (\((x1, y1), (x2, y2)) -> x1 == x2 || y1 == y2)

day5Task1 = fmap (M.size . M.filter (>= 2) . foldLines . straightLines) inputs

day5Task2 = fmap (M.size . M.filter (>= 2) . foldLines) inputs