module Graph(Graph, dfs, bfs) where

import Control.Monad(msum)

import Data.List(find)
-- https://monadmadness.wordpress.com/2014/11/10/purely-functional-graph-search-algorithms/

data Graph a = Node { label :: a
                    , connections :: [Graph a]
                    } deriving (Show)

prune :: Eq a => Graph a -> Graph a
prune g = prune' [] g
  where prune' ps (Node x xs) = if x `elem` ps
          then Node x []
          else Node x (map (prune' (x:ps)) xs)

dfs :: (a -> Bool) -> Graph a -> Maybe [a]
dfs p (Node x xs) = if p x
    then Just [x]
    else fmap (x:) . msum . map (dfs p) $ xs

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
