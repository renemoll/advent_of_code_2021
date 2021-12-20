module Day12 (solve) where

import Data.Char
import Data.List
import Data.Maybe
import Data.List.Split
import qualified Data.Map as Map

data Cave = Start
            | End
            | Small String
            | Big String
            deriving (Show, Eq, Ord)

toCave :: String -> Cave
toCave "start" = Start
toCave "end" = End
toCave all@(x:xs)
  | isLower x = Small all
  | otherwise = Big all

-- Create a map for each node to all connected nodes
parse :: String -> Map.Map Cave [Cave]
parse = Map.fromListWith (++) . concatMap parseConnection . lines
  where parseConnection = toTuple . map toCave . splitOn "-"
        toTuple [x,y] = [(x,[y]), (y,[x])]

-- Only small caves can be visited once, start and end cannot be visited.
-- Big caves can be visited multiple times
canVisit :: [Cave] -> Cave -> Bool
canVisit visited x = case x of
  Start -> False
  End -> False
  Big _ -> True
  Small _ -> not $ x `elem` visited

-- Same as canVisit, however now 1 small case may be visited twice
canVisitAlt :: (Map.Map Cave Int) -> Cave -> Bool
canVisitAlt visited x = case x of
  Start -> False
  End -> False
  Big _ -> True
  Small _ -> let
              isPresent = x `elem` (Map.keys visited)
              isSmall (Small _) = True
              isSmall _ = False
              isAvailable = Map.null $ Map.filterWithKey (\k v -> isSmall k && v > 1) visited
             in not isPresent || isAvailable

-- Go through the map and determine possible routes
visit :: (Map.Map Cave Int -> Cave -> Bool) -> Map.Map Cave [Cave] -> Int
visit p mp = go Map.empty Start End
  where go :: Map.Map Cave Int -> Cave -> Cave -> Int
        go history start end
          | end `elem` neighbours = subresult + 1
          | otherwise = subresult
          where neighbours = mp Map.! start
                visited = Map.insertWith (+) start 1 history
                options = filter (p visited) neighbours
                subresult = sum $ map (\a -> go visited a end) options

part1 :: Map.Map Cave [Cave] -> Int
part1 xs = visit (canVisit . Map.keys) xs

part2 :: Map.Map Cave [Cave] -> Int
part2 xs = visit canVisitAlt xs

solve :: String -> (Int, Int)
solve input = (s1, s2)
  where entries = parse input
        s1 = part1 entries -- 3450
        s2 = part2 entries -- 96528
