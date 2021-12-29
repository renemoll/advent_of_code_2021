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
        -- step: process a move into the forward and depth component
  where step [] f d = f * d
        step mvs f d
          | dir == "forward" = step (tail mvs) (f + a) d
          | dir == "down" = step (tail mvs) f (d + a)
          | dir == "up" = step (tail mvs) f (d - a)
          | otherwise = error "incorrect direction, valid options are: forward, down, up"
          where m = head mvs
                dir = direction m
                a = amount m

part2 :: [Movement] -> Int
part2 moves = step moves 0 0 0
  -- step: process a move into the forward, depth and aim component
  where step [] f d _ = f * d
        step mvs f d aim
          | dir == "forward" = step (tail mvs) (f + a) (d + aim * a) aim
          | dir == "down" = step (tail mvs) f d (aim + a)
          | dir == "up" = step (tail mvs) f d (aim - a)
          | otherwise = error "incorrect direction, valid options are: forward, down, up"
          where m = head mvs
                dir = direction m
                a = amount m

solve :: String -> (String, String)
solve input = (s1, s2)
  where entries = map parse $ lines input
        s1 = show $ part1 entries -- 1746616
        s2 = show $ part2 entries -- 1741971043
