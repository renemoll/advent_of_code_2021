module Day07 (solve) where

import Data.List.Split

parse :: String -> Int
parse = read :: String -> Int

-- Generate a list of numbers from x to y (both positive as negative)
generateRange :: Int -> Int -> [Int]
generateRange x y = if x <= y
                    then enumFromTo x y
                    else reverse $ enumFromTo y x

-- Determine the difference between a fixed number and all elements in a list
diff :: Int -> [Int] -> [Int]
diff n = map (abs . subtract n)

part1 :: [Int] -> Int
part1 xs = minimum $ map (\a -> sum $ diff a xs) range
  where range = generateRange (minimum xs) (maximum xs)

-- Calculate the costs for each movement
costs :: [Int] -> [Int]
costs = map (sum . generateRange 0)

part2 :: [Int] -> Int
part2 xs = minimum $ map (\a -> sum $ costs $ diff a xs) range
  where range = generateRange (minimum xs) (maximum xs)

solve :: String -> (String, String)
solve input = (s1, s2)
  where entries = map parse $ splitOn "," input
        s1 = show $ part1 entries -- 356958
        s2 = show $ part2 entries -- 105461913
