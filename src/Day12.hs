{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Day12 where

import FileAccess
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Char (isLower)
import Data.Maybe (fromJust)

type VertexKey = String
data Visited = Zero | Once | Twice deriving Show
data VertexType = Small | Big deriving Show

data VertexData
  = VertexData Visited VertexType
  | StartVertex
  | EndVertex deriving Show

type Vertices = M.Map VertexKey VertexData
type Edges = M.Map VertexKey [VertexKey]

data Graph = Graph Vertices Edges deriving Show

inputs = fmap ((\l -> (T.unpack $ head l, T.unpack $ l !! 1)) . T.splitOn "-") <$> readLines "resources/Day12Input.txt"

emptyGraph = Graph M.empty M.empty

visitVertex :: VertexKey -> Graph -> Graph
visitVertex v (Graph vertices edges) = Graph (M.alter (\case Nothing -> Nothing
                                                             Just (VertexData Zero t) -> Just (VertexData Once t)
                                                             Just (VertexData Once t) -> Just (VertexData Twice t)
                                                             Just x -> Just x) v vertices) edges

smallCaveVisitedMoreThanOnce :: Graph -> Bool
smallCaveVisitedMoreThanOnce (Graph vertices edges) = any (\case VertexData Twice Small -> True
                                                                 _ -> False) (M.elems vertices)

buildGraph :: [(String, String)] -> Graph
buildGraph = foldl go emptyGraph
  where go :: Graph -> (String, String) -> Graph
        go g@(Graph vertices edges) (x,y) = Graph (pushVertex x $ pushVertex y vertices) (pushEdges x y edges)
        pushVertex :: VertexKey -> Vertices -> Vertices
        pushVertex key vertices
          | M.member key vertices = vertices
          | key == "start" = M.insert key StartVertex vertices
          | key == "end" = M.insert key EndVertex vertices
          | all isLower key = M.insert key (VertexData Zero Small) vertices
          | otherwise = M.insert key (VertexData Zero Big) vertices
        pushEdges :: VertexKey -> VertexKey -> Edges -> Edges
        pushEdges v1 v2 edges = M.alter (alterFun v1) v2 $ M.alter (alterFun v2) v1 edges
          where alterFun :: VertexKey -> Maybe [VertexKey] -> Maybe [VertexKey]
                alterFun v Nothing = Just [v]
                alterFun v (Just xs) = Just (v:xs)

findPathsCount1 :: Graph -> VertexKey -> Integer
findPathsCount1 graph@(Graph vertices edges) key =
  case fromJust $ M.lookup key vertices of
    StartVertex -> sum $ map (findPathsCount1 graph) (fromJust $ M.lookup key edges)
    EndVertex -> 1
    (VertexData Once Small) -> 0
    (VertexData Twice Small) -> 0
    (VertexData _ _) -> goRecurse
  where filterOutNotWorthVisiting :: VertexKey -> Bool
        filterOutNotWorthVisiting v = case fromJust $ M.lookup v vertices of
          StartVertex -> False
          (VertexData Once Small) -> False
          _ -> True
        goRecurse = sum $ map (findPathsCount1 (visitVertex key graph)) (filter filterOutNotWorthVisiting $ fromJust $ M.lookup key edges)

findPathsCount2 :: Graph -> VertexKey -> Integer
findPathsCount2 graph@(Graph vertices edges) key =
  case fromJust $ M.lookup key vertices of
    StartVertex -> sum $ map (findPathsCount2 graph) (fromJust $ M.lookup key edges)
    EndVertex -> 1
    (VertexData Once Small) -> if not $ smallCaveVisitedMoreThanOnce graph then goRecurse else 0
    (VertexData Twice Small) -> 0
    (VertexData _ _) -> goRecurse
  where filterOutNotWorthVisiting :: VertexKey -> Bool
        filterOutNotWorthVisiting v = case fromJust $ M.lookup v vertices of
          StartVertex -> False
          (VertexData Twice Small) -> not $ smallCaveVisitedMoreThanOnce graph
          _ -> True
        goRecurse = sum $ map (findPathsCount2 (visitVertex key graph)) (filter filterOutNotWorthVisiting $ fromJust $ M.lookup key edges)

day12Task1 = fmap ((`findPathsCount1` "start") . buildGraph) inputs

day12Task2 = fmap ((`findPathsCount2` "start") . buildGraph) inputs