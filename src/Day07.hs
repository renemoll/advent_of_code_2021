module Day07 (solve) where

import Data.List
import Data.List.Split

parse :: String -> [Int]
parse xs = map (read :: String -> Int) $ splitOn "," xs

-- Generate a list of numbers from x to y (both positive as negative)
generateRange :: Int -> Int -> [Int]
generateRange x y = if x <= y
                    then enumFromTo x y
                    else reverse $ enumFromTo y x

-- Determine the difference between a fixed number and all elements in a list
diff :: Int -> [Int] -> [Int]
diff n xs = map (abs . subtract n) xs

part1 :: [Int] -> Int
part1 xs = lowest
  where range = generateRange (minimum xs) (maximum xs)
        result = map (\a -> sum $ diff a xs) range
        lowest = minimum result

-- Calculate the costs for each movement
costs :: [Int] -> [Int]
costs xs = map (\a -> sum $ generateRange 0 a) xs

part2 :: [Int] -> Int
part2 xs = lowest
  where range = generateRange (minimum xs) (maximum xs)
        result = map (\a -> sum $ costs $ diff a xs) range
        lowest = minimum result

solve :: String -> (Int, Int)
solve input = (s1, s2)
  where entries = parse input
        s1 = part1 entries -- 356958
        s2 = part2 entries -- 105461913
