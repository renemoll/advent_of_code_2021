module Day02 (solve) where

data Movement = Movement {
  direction :: String,
  amount :: Int
} deriving (Show)

parse :: String -> Movement
parse input = Movement d a
  where [d, n] = words input
        a = read n

part1 :: [Movement] -> Int
part1 moves = step moves 0 0
  where step [] f d = f * d
        step ms f d
          | dir == "forward" = step (tail ms) (f + a) d
          | dir == "down" = step (tail ms) f (d + a)
          | dir == "up" = step (tail ms) f (d - a)
          | otherwise = error "incorrect direction, valid options are: forward, down, up"
          where m = head ms
                dir = direction m
                a = amount m

part2 :: [Movement] -> Int
part2 moves = step moves 0 0 0
  where step [] f d _ = f * d
        step ms f d aim
          | dir == "forward" = step (tail ms) (f + a) (d + aim * a) aim
          | dir == "down" = step (tail ms) f d (aim + a)
          | dir == "up" = step (tail ms) f d (aim - a)
          | otherwise = error "incorrect direction, valid options are: forward, down, up"
          where m = head ms
                dir = direction m
                a = amount m

solve :: String -> (Int, Int)
solve input = (s1, s2)
  where entries = map parse $ lines input
        s1 = part1 entries -- 1746616
        s2 = part2 entries -- 1741971043
