module Graph (Graph, dfs, bfs) where

import Data.Foldable (asum)

import Data.List(find)

-- https://monadmadness.wordpress.com/2014/11/10/purely-functional-graph-search-algorithms/


-- This denotes a connected graph
data Graph a = Node { label :: a
                    , connections :: [Graph a]
                    } deriving (Show)

prune :: Eq a => Graph a -> Graph a
prune g = prune' [] g
  where prune' ps (Node x xs) = if x `elem` ps
          then Node x []
          else Node x (map (prune' (x:ps)) xs)

dfs :: (a -> Bool) -> Graph a -> Maybe [a]
dfs p (Node x xs)
  | p x = Just [x]
  | otherwise = fmap (x:) . asum . map (dfs p) $ xs

bfs :: (a -> Bool) -> Graph a -> Maybe [a]
bfs p = find (p . last) . rankedPaths
    where
        rankedPaths :: Graph a -> [[a]]
        rankedPaths (Node x xs) = ([x]:) . map (x:) . foldr mergeRanked [] . map rankedPaths $ xs

        mergeRanked :: [[a]] -> [[a]] -> [[a]]
        mergeRanked [] ys = ys
        mergeRanked xs [] = xs
        mergeRanked (x:xs) (y:ys) = if length x < length y
            then x : (mergeRanked xs (y:ys))
            else y : (mergeRanked (x:xs) ys)

-- A general unconnected graph
type Graph1 a = [Graph a]

dfs1 :: (a -> Bool) -> Graph1 a -> Maybe [a]
dfs1 p = asum . map (dfs p)

bfs1 :: (a -> Bool) -> Graph1 a -> Maybe [a]
bfs1 p = asum . map (bfs p)
