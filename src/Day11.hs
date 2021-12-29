module Day11 (solve) where

parse :: String -> String
parse x = x

part1 :: String -> Int
part1 _ = 0

part2 :: String -> Int
part2 _ = 0

solve :: String -> (String, String)
solve input = (s1, s2)
  where entries = parse input
        s1 = show $ part1 entries -- ?
        s2 = show $ part2 entries -- ?
