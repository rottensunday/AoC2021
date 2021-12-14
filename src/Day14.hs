{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Day14 where

import FileAccess
import qualified Data.Text as T
import Text.Megaparsec
import Data.Void (Void)
import Text.Megaparsec.Char (newline, char, string, alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)
import Data.Either (fromRight)
import qualified Data.Map as M
import Data.List

type Parser = Parsec Void T.Text
type Template = String
type TemplateMap = M.Map String Integer
data Insertion = Insertion String Char deriving Show
type InsertionMap = M.Map String Char

inputs = fmap (fromRight ("", M.empty) . parse parseInput "") (readWholeFile "resources/Day14Input.txt")

parseInput :: Parser (Template, InsertionMap)
parseInput = do
  template <- many alphaNumChar
  many $ void newline
  insertions <- many (Insertion <$> (many alphaNumChar <* string " -> ") <*> alphaNumChar <* (void newline <|> eof))
  return (template, buildInsertionMap insertions)

buildInsertionMap :: [Insertion] -> InsertionMap
buildInsertionMap [] = M.empty
buildInsertionMap ((Insertion str c):xs) = M.union (M.singleton str c) (buildInsertionMap xs)

step :: InsertionMap -> TemplateMap -> TemplateMap
step insMap temMap = M.foldrWithKey processWithInsertedPair M.empty withInsertedPairs
  where withInsertedPairs = M.mapKeys (\k@[c1,c2] -> case M.lookup k insMap of
                                                      Nothing -> k
                                                      Just c -> [c1,c,c2]) temMap
        processWithInsertedPair :: [Char] -> Integer -> TemplateMap -> TemplateMap
        processWithInsertedPair k@[c1,c2] val m = M.alter (modFun val) k m
        processWithInsertedPair [c1,c2,c3] val m = M.alter (modFun val) [c2,c3] $ M.alter (modFun val) [c1,c2] m
        processWithInsertedPair _ _ _ = error "Unexpected"
        modFun val = \case Nothing -> Just val
                           Just x -> Just $ x + val

toPairsMap :: Template -> (TemplateMap, (Char, Char))
toPairsMap template = (foldl (flip (M.alter (\case Nothing -> Just 1
                                                   Just x -> Just (x+1)))) M.empty pairs, (head template, last template))
  where pairs = zipWith (\x y -> [x,y]) template (drop 1 template)

fromPairsMapToOccurences :: (TemplateMap, (Char, Char)) -> M.Map Char Integer
fromPairsMapToOccurences (tempMap, (startLetter, endLetter)) =
  M.map (`div` 2)
  $ M.alter addLetterFn endLetter
  $ M.alter addLetterFn startLetter
  $ M.foldrWithKey processPair M.empty tempMap
  where processPair :: [Char] -> Integer -> M.Map Char Integer -> M.Map Char Integer
        processPair cs val m = foldl (flip (M.alter (\case Nothing -> Just val
                                                           Just x -> Just $ x + val))) m cs
        addLetterFn = \case Nothing -> Just 1
                            Just x -> Just $ x+1

makeNSteps :: Int -> Template -> InsertionMap -> M.Map Char Integer
makeNSteps n temp imap = 
  let (tempMap, (start, end)) = toPairsMap temp 
    in fromPairsMapToOccurences (iterate (step imap) tempMap !! n, (start, end))

maxMinDifference :: M.Map Char Integer -> Integer
maxMinDifference = (\l -> last l - head l) . sort . M.elems

day14Task1 = fmap (maxMinDifference . uncurry (makeNSteps 10)) inputs

day14Task2 = fmap (maxMinDifference . uncurry (makeNSteps 40)) inputs