module Day02 (solve) where

data Movement = Movement {
  direction :: String,
  amount :: Int
} deriving (Show)

parse :: String -> Movement
parse input = Movement direction amount
  where [direction, number] = words input
        amount = read number :: Int

calcMovement moves = move moves 0 0
  where move moves f d
          | null moves = f*d
          | dir == "forward" = move (tail moves) (f + a) d
          | dir == "down" = move (tail moves) f (d + a)
          | dir == "up" = move (tail moves) f (d - a)
          where m = head moves
                dir = direction m
                a = amount m

calcMovement2 moves = move moves 0 0 0
  where move moves f d aim
          | null moves = f*d
          | dir == "forward" = move (tail moves) (f + a) (d + aim * a) aim
          | dir == "down" = move (tail moves) f d (aim + a)
          | dir == "up" = move (tail moves) f d (aim - a)
          where m = head moves
                dir = direction m
                a = amount m

solve :: String -> (Int, Int)
solve input = (s1, s2)
  where numbers = map parse $ lines input
        s1 = calcMovement numbers
        s2 = calcMovement2 numbers
