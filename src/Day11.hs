module Day11 (solve) where

part1 _ = 0

part2 _ = 0

solve :: String -> (String, String)
solve input = (s1, s2)
  where entries = parse input
        s1 = show $ part1 entries -- ?
        s2 = show $ part2 entries -- ?
