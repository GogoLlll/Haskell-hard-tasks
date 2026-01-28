module Main where

import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.Set (Set)

dikstra
  :: (Int -> [(Int, Int)])
  -> (Int -> Bool)  
  -> Int
  -> Map Int Int  
dikstra graph stop start =
  go initialDist S.empty
  where
    initialDist :: Map Int Int
    initialDist = M.singleton start 0

    go :: Map Int Int -> Set Int -> Map Int Int
    go dist visited =
      case pickMin dist visited of
        Nothing ->
          dist 
        Just (v, d)
          | stop v ->
              dist
          | otherwise ->
              let
                visited' = S.insert v visited
                dist'    = relaxNeighbors v d dist visited'
              in
                go dist' visited'

pickMin
  :: Map Int Int
  -> Set Int
  -> Maybe (Int, Int)
pickMin dist visited =
  let unvisited =
        M.filterWithKey (\v _ -> not (S.member v visited)) dist
  in M.lookupMin unvisited

relaxNeighbors
  :: Int
  -> Int
  -> Map Int Int
  -> Set Int
  -> Map Int Int
relaxNeighbors v d dist visited =
  foldl step dist (graph v)
  where
    step m (u, w)
      | S.member u visited = m
      | otherwise =
          let newDist = d + w
          in case M.lookup u m of
               Nothing ->
                 M.insert u newDist m
               Just old
                 | newDist < old ->
                     M.insert u newDist m
                 | otherwise ->
                     m

graph :: Int -> [(Int, Int)]
graph 1 = [(2, 7), (3, 9)]
graph 2 = [(3, 10), (4, 15)]
graph 3 = [(4, 11)]
graph 4 = []
graph _ = []

main :: IO ()
main = do
  let result = dikstra graph (const False) 1
  putStrLn "Shortest paths from 1:"
  mapM_ print (M.toList result)