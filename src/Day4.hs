{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Day4 where

import FileAccess
import Data.Functor ((<$))
import Text.Megaparsec
import Data.Void (Void)
import qualified Data.Text as T
import Text.Megaparsec.Char (hspace, newline, char)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void, replicateM, join)
import Data.List (find)
import Data.Maybe (fromMaybe, isNothing)
import Data.Either (fromRight)

type Value = Int
type Checked = Bool
type Board = [[(Value, Checked)]]
type BingoNumber = Int
type BingoNumbers = [BingoNumber]
type Result = Int

type Parser = Parsec Void T.Text

inputs = readWholeFile "resources/Day4Input.txt"

parseInputs :: T.Text -> (BingoNumbers, [Board])
parseInputs x = fromRight ([], []) (parse inputParser "" x)

boardParser :: Parser Board
boardParser = do
  replicateM 5 do
    void $ many newline
    row <- replicateM 5 (fmap (, False) (hspace *> L.decimal <* hspace))
    void $ many newline
    return row

bingoNumbersParser :: Parser BingoNumbers
bingoNumbersParser = many (L.decimal <* many (char ','))

inputParser :: Parser (BingoNumbers, [Board])
inputParser = do
  nums <- bingoNumbersParser
  void newline
  boards <- many boardParser
  return (nums, boards)

bingoStep :: BingoNumber -> [Board] -> [Board]
bingoStep n = (map . map . map) (\(x,y) -> if x == n then (x, True) else (x, y))

bingoStep2 :: BingoNumber -> [Board] -> [Board]
bingoStep2 n boards = filteredBoards
  where newBoards = bingoStep n boards
        filteredBoards = filter (isNothing . checkBoard) newBoards

checkBoard :: Board -> Maybe Result
checkBoard board = if not $ go 0 then Nothing else Just unmarkedSum
  where checkRow i = (== 5) . length . filter snd $ board !! i
        checkColumn i = (== 5) . length . filter snd $ map (!! i) board
        go 5 = False
        go i = checkRow i || checkColumn i || go (i+1)
        unmarkedSum = sum . map fst . filter (not . snd) . concat $ board

processBingoNumbers :: BingoNumbers -> [Board] -> Result
processBingoNumbers ns boards = fromMaybe (-1) $ snd $ foldl processNumber (boards, Nothing) ns
  where processNumber :: ([Board], Maybe Result) -> BingoNumber -> ([Board], Maybe Result)
        processNumber x@(boards', Just res) n = x
        processNumber x@(boards', Nothing) n =
          let (newBoards, result) = (bingoStep n boards', checkBoards newBoards n)
            in (newBoards, result)
        checkBoards boards' n = fmap (*n) $ join $ find (\case
                                      Just x -> True
                                      Nothing -> False) $ map checkBoard boards'

processBingoNumbers2 :: BingoNumbers -> [Board] -> Result
processBingoNumbers2 ns boards = fromMaybe (-1) $ snd $ foldl processNumber (boards, Nothing) ns
  where processNumber :: ([Board], Maybe Result) -> BingoNumber -> ([Board], Maybe Result)
        processNumber x@([board], Just res) n = ([board], Just res)
        processNumber x@([board], Nothing) n = 
          let (newBoards, result) = (bingoStep n [board], checkBoards newBoards n)
            in ([board], result)
        processNumber x@(boards', _) n = (bingoStep2 n boards', Nothing)
        checkBoards boards' n = fmap (*n) $ join $ find (\case
                                      Just x -> True
                                      Nothing -> False) $ map checkBoard boards'

day4Task1 = fmap
              (\x -> let (bingoNumbers, boards) = parseInputs x in processBingoNumbers bingoNumbers boards)
              inputs

day4Task2 = fmap
              (\x -> let (bingoNumbers, boards) = parseInputs x in processBingoNumbers2 bingoNumbers boards)
              inputs