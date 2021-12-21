module Day06 (solve) where

import Data.List
import Data.List.Split

parse :: String -> Int
parse = read :: String -> Int

--
-- Solution 1: naive
--
-- Given a list of days representing the days before a fish births a new fish,
-- update each fish's counter and add the new fishes to the list.
--
iterateDay :: [Int] -> [Int]
iterateDay [] = []
iterateDay (x:xs) = go x ++ iterateDay xs
  where go y
          | y == 0 = [6, 8]
          | otherwise = [y - 1]

part1 :: Int -> [Int] -> Int
part1 0 xs = length xs
part1 n xs = part1 (n - 1) $ iterateDay xs

--
-- Solution 2: histogram
--
-- The first solution does not scale very nicely. So instead of keeping track
-- of each fish individually, keep track of how many fish are at which specific 'age'.
--
entries2hist :: [Int] -> [Int]
entries2hist xs = map (\a -> a -1) zs
  where ys = [0..8]
        zs = map length (group (sort $ xs ++ ys))

update :: [Int] -> [Int]
update [] = []
update (x:xs) = pre ++ [y + x] ++ ys ++ [x]
  where (pre,y:ys) = splitAt 6 xs

part2 :: Int -> [Int] -> Int
part2 n xs = go n $ entries2hist xs
  where go 0 ys = sum ys
        go i ys = go (i - 1) $ update ys

solve :: String -> (String, String)
solve input = (s1, s2)
  where entries = map parse $ splitOn "," input
        s1 = show $ part1 80 entries -- 350149
        s2 = show $ part2 256 entries -- 1590327954513
