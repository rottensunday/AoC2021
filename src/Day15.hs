{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Day15 where

import FileAccess
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Char (digitToInt)

type Point = (Int, Int)
type Risk = Int
type Graph = M.Map Point Int
type ShortestPathWeights = Graph

inputs = readWholeFile "resources/Day15Input.txt"

parseInputs :: T.Text -> Graph
parseInputs t = foldl foldLinesFn M.empty (zip (T.lines t) [0..])
  where foldLinesFn :: Graph -> (T.Text, Int) -> Graph
        foldLinesFn g (line, i) = foldl (\accMap' (c, j) -> foldLineFn accMap' c (i,j)) g (zip (T.unpack line) [0..])
        foldLineFn :: Graph -> Char -> Point -> Graph
        foldLineFn g c p = M.insert p (digitToInt c) g

emptyWeights :: Graph -> ShortestPathWeights
emptyWeights = M.map (const 0)

relaxNeighbors :: Point -> Graph -> ShortestPathWeights -> ShortestPathWeights
relaxNeighbors p g spw = undefined

neighbors :: Point -> Graph -> [Point]
neighbors p@(x,y) g = (flip $ M.lookup g) [(x-1,y), (x+1,y), (x,y-1), (x,y+1)]