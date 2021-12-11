{-# LANGUAGE TupleSections #-}
module Day11 where

import qualified Data.Text as T
import FileAccess
import Data.Char (digitToInt)
import qualified Data.Vector as V

type EnergyLevel = Int
type Board = V.Vector (V.Vector EnergyLevel)
type DidFlash = Bool
type BoardWithFlashes = V.Vector (V.Vector (EnergyLevel, DidFlash))
type Coordinate = (EnergyLevel, EnergyLevel)

inputs = fmap V.fromList $ fmap (V.fromList . map digitToInt . T.unpack) <$> readLines "resources/Day11Input.txt"

increaseEnergyLevels :: Board -> Board
increaseEnergyLevels = (fmap . fmap) (+1)

modifyElemValue :: V.Vector (V.Vector a) -> (Int, Int) -> (a -> a) -> V.Vector (V.Vector a)
modifyElemValue b (x,y) f = V.imap (\i val -> if i == x then V.imap (\j val' -> if j == y then f val' else val') val else val) b

processFlashes :: Board -> BoardWithFlashes
processFlashes board = go ((fmap . fmap) (, False) board)
  where go :: BoardWithFlashes -> BoardWithFlashes
        go board' = let tens = find10s board' in
                      if V.null tens then board'
                      else go $ flip update10sNeighbors tens $ foldl (\board'' coord -> modifyElemValue board'' coord (\(q,w) -> (0, True))) board' tens

clearFlashes :: BoardWithFlashes -> BoardWithFlashes
clearFlashes = (fmap . fmap) (\x -> if snd x then (0, True) else x)

find10s :: BoardWithFlashes -> V.Vector Coordinate
find10s board' = V.concat $ V.toList $ V.imap (\i l -> (i,) <$> V.findIndices (\(x,y) -> x >= 10 && not y) l) board'

update10sNeighbors :: BoardWithFlashes -> V.Vector Coordinate -> BoardWithFlashes
update10sNeighbors board coords
  | null coords = board
  | otherwise = update10sNeighbors (foldl modifyNeighbor board neighbors) (V.tail coords)
  where neighbors = neighborhoodCoords board (V.head coords)
        modifyNeighbor = \board' coord -> modifyElemValue board' coord (\(q,w) -> (q+1, w))

neighborhoodCoords :: V.Vector (V.Vector a) -> Coordinate -> V.Vector Coordinate
neighborhoodCoords board (x,y) = V.fromList $ filter filterCoord [(x', y') | x' <- [x-1..x+1], y' <- [y-1..y+1]]
  where top = 0
        left = 0
        right = V.length (V.head board) - 1
        bottom = V.length board - 1
        filterCoord :: Coordinate -> Bool
        filterCoord (x',y') = (x', y') /= (x,y) && x' >= top && y' >= left && x' <= bottom && y' <= right

step :: Bool -> (Board, Int) -> (Board, Int)
step shouldSum (board, nFlashes) = let resultBoard = clearFlashes . processFlashes . increaseEnergyLevels $ board
                          in ((fmap . fmap) fst resultBoard,
                              (if shouldSum then nFlashes else 0) + V.sum (fmap (length . V.filter snd) resultBoard))

day11Task1 = fmap (\x -> V.iterateN 101 (step True) (x,0) V.! 100) inputs

day11Task2 = fmap (\x -> V.findIndex (\(b, i) -> i == 100) $ V.iterateN 1000 (step False) (x,0)) inputs