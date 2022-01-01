module Day15 (solve) where

import Data.Maybe
import qualified Data.Map as Map
import qualified Data.Set as Set

type Coordinate = (Int, Int)
type Graph = Map.Map Coordinate Int
type Lookup =  Map.Map Int (Set.Set Coordinate)

parse :: String -> Graph
parse = Map.fromList . foldMap (\(x,l) -> map (\(y,v) -> ((x, y), read [v])) l)
    . zip [0..] . map (zip [0..]) . lines

getNeighbours :: Graph -> Coordinate -> [Coordinate]
getNeighbours riskMap (x,y) = [fromJust r | r <- result, isJust r]
  where ((rows, cols), _) = fromJust $ Map.lookupMax riskMap
        result = let
                  left = if x > 0 then Just (x - 1, y) else Nothing
                  right = if x < cols then Just (x + 1, y) else Nothing
                  up = if y > 0 then Just (x, y - 1) else Nothing
                  down = if y < rows then Just (x, y + 1) else Nothing
                  in [left, right, up, down]

calcRisk :: Graph -> Coordinate -> Int
calcRisk riskMap end = go (Map.singleton 0 (Set.singleton (0,0))) Set.empty
  where go :: Lookup -> Set.Set Coordinate -> Int
        go vQ visited
          | end `elem` minCoordinates = minRisk
          | otherwise = go alt visitedUnique
          where ((minRisk, minCoordinates), newQ) = fromJust $ Map.minViewWithKey vQ
                neighbours = filter (`notElem` visited) $ concatMap (getNeighbours riskMap) minCoordinates
                scorePoint :: Coordinate -> Int
                scorePoint p = minRisk + riskMap Map.! p
                alt = foldr (\p acc -> Map.insertWith Set.union (scorePoint p) (Set.singleton p) acc) newQ neighbours
                visitedUnique = Set.union visited $ Set.filter (`notElem` visited) minCoordinates

part1 :: Graph -> Int
part1 xs = calcRisk xs destination
  where (destination, _) = fromJust $ Map.lookupMax xs

expandMap :: Graph -> Int -> Graph
expandMap riskMap n = foldr Map.union Map.empty [go (x, y) | x <- [0..(n-1)], y <- [0..(n-1)]]
  where ((l,_), _) = fromJust $ Map.lookupMax riskMap
        shiftMap :: (Int, Int) -> Graph
        shiftMap (x0, y0) = Map.mapKeys (\(x,y) -> (x0 + x, y0 + y)) riskMap
        increaseMap :: Int -> Graph -> Graph
        increaseMap x = Map.map (\a -> if (a + x) <= 9 then a + x else ((a + x) `mod` 10) + 1)
        go (x,y) = increaseMap (x + y) $ shiftMap s
          where s = ((l + 1) * x, (l + 1) * y)

part2 :: Graph -> Int
part2 xs = calcRisk extendedMap destination
  where extendedMap = expandMap xs 5
        (destination, _) = fromJust $ Map.lookupMax extendedMap

solve :: String -> (String, String)
solve input = (s1, s2)
  where entries = parse input
        s1 = show $ part1 entries -- 755
        s2 = show $ part2 entries -- ?
