module FloydWarshall where

import Data.IntMap.Strict as Map hiding (foldl',foldr)
import Data.List

--| This isn't a purely functional data structure. But contains a purely functional implementation
    of the Floyd Warshall's shortest path algorithm. The representation of the graph is a map of
    each node to a map of its adjacent node to its weight.

type Vertex = Int
type Weight = Int

type Graph = IntMap (IntMap Weight)

weight :: Graph -> Vertex -> Vertex -> Maybe Weight
weight g i j = do
  jmap <- Map.lookup i g
  Map.lookup j jmap

shortestPaths :: [Vertex] -> Graph -> Graph
shortestPaths vs g = foldl' update g vs
  where
    update :: Graph -> Vertex -> Graph
    update g k = mapWithKey shortmap g
     where
      shortmap :: Key -> IntMap Weight -> IntMap Weight
      shortmap i jmap = foldr shortest Map.empty vs
       where shortest j m =
               case (old,new) of
                   (Nothing, Nothing) -> m
                   (Nothing, Just w ) -> Map.insert j w m
                   (Just w,  Nothing) -> Map.insert j w m
                   (Just w1, Just w2) -> Map.insert j (min w1 w2) m
                where
                  old = Map.lookup j jmap
                  new = do w1 <- weight g i k
                           w2 <- weight g k j
                           return (w1+w2)
