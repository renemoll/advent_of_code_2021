module Day12 (solve) where

import Data.Char
import Data.List.Split
import qualified Data.Map as Map

data Cave = Start
            | End
            | Small String
            | Big String
            deriving (Show, Eq, Ord)

type CaveMap = Map.Map Cave [Cave]
type VisitedMap = Map.Map Cave Int

toCave :: String -> Cave
toCave "start" = Start
toCave "end" = End
toCave name@(x:_)
  | isLower x = Small name
  | otherwise = Big name
toCave _ = error "Invalid argument"

-- Create a map for each node to all connected nodes
parse :: String -> CaveMap
parse = Map.fromListWith (++) . concatMap parseConnection . lines
  where parseConnection = toTuple . map toCave . splitOn "-"
        toTuple [x,y] = [(x,[y]), (y,[x])]
        toTuple _ = error "Invalid argument"

-- Only small caves can be visited once, start and end cannot be visited.
-- Big caves can be visited multiple times
canVisit :: [Cave] -> Cave -> Bool
canVisit visited x = case x of
  Start -> False
  End -> False
  Big _ -> True
  Small _ -> x `notElem` visited

-- Same as canVisit, however now 1 small case may be visited twice
canVisitAlt :: VisitedMap -> Cave -> Bool
canVisitAlt visited x = case x of
  Start -> False
  End -> False
  Big _ -> True
  Small _ -> let
              isPresent = x `elem` Map.keys visited
              isSmall (Small _) = True
              isSmall _ = False
              isAvailable = Map.null $ Map.filterWithKey (\k v -> isSmall k && v > 1) visited
             in not isPresent || isAvailable

-- Go through the map and determine possible routes
visit :: (VisitedMap -> Cave -> Bool) -> CaveMap -> Int
visit p mp = go Map.empty Start End
  where go :: VisitedMap -> Cave -> Cave -> Int
        go history start end
          | end `elem` neighbours = subresult + 1
          | otherwise = subresult
          where neighbours = mp Map.! start
                visited = Map.insertWith (+) start 1 history
                options = filter (p visited) neighbours
                subresult = sum $ map (\a -> go visited a end) options

part1 :: CaveMap -> Int
part1 = visit (canVisit . Map.keys)

part2 :: CaveMap -> Int
part2 = visit canVisitAlt

solve :: String -> (String, String)
solve input = (s1, s2)
  where entries = parse input
        s1 = show $ part1 entries -- 3450
        s2 = show $ part2 entries -- 96528
