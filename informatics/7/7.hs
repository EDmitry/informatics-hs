import Data.Array (Array, listArray, array, bounds, assocs, (!), (//))
import Data.Array.ST
import Data.Ix (range)
import Data.Maybe
import Data.Function (on)
import Control.Monad (replicateM)

type Graph = Array Int [(Int, Int)]

addEdge :: Graph -> Int -> Int -> Int -> Graph
addEdge graph i j w = graph // [(i, (j, w):(graph!i))]

extractMinBy :: (a -> a -> Bool) -> [a] -> (a, [a])
extractMinBy f (x:xs) = extractMinBy' f [x] xs (x, xs)

extractMinBy' :: (a -> a -> Bool) -> [a] -> [a] -> (a, [a]) -> (a, [a])
extractMinBy' f left [] r = r
extractMinBy' f left (r:right) z@(x, xs) = extractMinBy' f (left ++ [r]) right newResult
  where newResult = if f r x then (r, left ++ right) else z

distanceLT :: Maybe Int -> Maybe Int -> Bool
distanceLT Nothing Nothing = False
distanceLT Nothing _ = False
distanceLT _ Nothing = True
distanceLT (Just a) (Just b) = a < b

dijkstra :: Graph -> Int -> Array Int (Maybe Int)
dijkstra graph start = dijkstraStep graph distances (range $ bounds graph)
  where distances = listArray (bounds graph) (repeat Nothing) // [(start, Just 0)]
  
dijkstraStep :: Graph -> Array Int (Maybe Int) -> [Int] -> Array Int (Maybe Int)
dijkstraStep _ distances [] = distances
dijkstraStep graph distances xs = dijkstraStep graph newDistances rest
  where (v, rest) = extractMinBy (distanceLT `on` (!) distances) xs
        newDistances = distances // foldl (\relaxed (w, d) ->
                                            let newDistance = fmap (+d) (distances!v) in
                                            if newDistance `distanceLT` (distances!w) then (w, newDistance):relaxed
                                            else relaxed
                                          ) [] (graph!v)

main = do n <- fmap read getLine
          ps <- fmap (map read . words) getLine
          let psa = listArray (1, n) ps
          m <- fmap read getLine
          edges <- replicateM m (fmap (map read . words) getLine)
          let graph' = foldl (\g [i, j] -> addEdge g i j (psa!i)) (listArray (1, n) (repeat [])) edges
          let graph = foldl (\g [i, j] -> addEdge g j i (psa!j)) graph' edges
          print $ fromMaybe (-1) (dijkstra graph 1 ! n)
