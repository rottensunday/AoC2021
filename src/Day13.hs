{-# LANGUAGE OverloadedStrings #-}

module Day13 where

import FileAccess
import qualified Data.Text as T
import Text.Megaparsec
import Data.Void (Void)
import Text.Megaparsec.Char (newline, char, string)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)
import Data.Either (fromRight)
import Data.List (nub)

type Parser = Parsec Void T.Text
type Point = (Int, Int)
data Fold = XFold Int | YFold Int deriving Show

inputs = fmap (fromRight ([], []) . parse parseInput "") (readWholeFile "resources/Day13Input.txt")

parseInput :: Parser ([Point], [Fold])
parseInput = do
  points <- many $ (,) <$> (L.decimal <* char ',') <*> (L.decimal <* newline)
  void newline
  folds <- 
    many $ string "fold along " *>
    (XFold <$> (string "x=" *> L.decimal) <|> YFold <$> (string "y=" *> L.decimal)) 
    <* (void newline <|> eof)
  return (points, folds)

processFold :: [Point] -> Fold -> [Point]
processFold [] _ = []
processFold ((x,y):xs) fold@(YFold i)
  | y <= i = (x,y):processFold xs fold
  | otherwise = let dist = 2*(y-i) in (x,y-dist):processFold xs fold
processFold ((x,y):xs) fold@(XFold i)
  | x <= i = (x,y):processFold xs fold
  | otherwise = let dist = 2*(x-i) in (x-dist,y):processFold xs fold

day13Task1 = fmap (\(points, folds) -> length $ nub $ processFold points (head folds)) inputs

day13Task2 = (printNicely . uncurry (foldl (\ ps f -> nub $ processFold ps f))) =<< inputs

printNicely :: [Point] -> IO ()
printNicely points = mapM_ putStrLn (foldl setPoint emptyTable points)
  where emptyTable = replicate 6 (replicate 39 '.')

setPoint :: [[Char]] -> Point -> [[Char]]
setPoint l (x,y) = let (l', r:rs) = splitAt y l in l' ++ (let (l'', r':rs') = splitAt x r in l'' ++ '#':rs'):rs