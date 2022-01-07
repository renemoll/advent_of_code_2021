module Day17 (solve) where

import Data.List.Split

type TargetArea = ((Int,Int), (Int,Int))

parse :: String -> TargetArea
parse x = (toTuple xs, toTuple ys)
  where p = words x
        xs = map (read :: String -> Int) $ splitOn ".." $ init $ drop 2 $ p !! 2
        ys = map (read :: String -> Int) $ splitOn ".." $ drop 2 $ p !! 3
        toTuple [x,y] = (x,y)
        toTuple _ = error "Invalid argument"

part1 :: TargetArea -> Int
part1 (_, (y0, _)) = n * (n - 1) `div` 2
  where n = -1 * y0

roots :: Floating a => a -> a -> a -> [a]
roots a b c = [x1, x2]
  where x1 = (-b + sqrt(b^2 - 4*a*c)) / (2*a)
        x2 = (-b - sqrt(b^2 - 4*a*c)) / (2*a)

hitTarget :: TargetArea -> (Int, Int) -> Bool
hitTarget ((x0, x1), (y0, y1)) (dx, dy) = any (==True) valid
  where x = scanl1 (+) [dx - c | c <- [0..(dx - 1)]]
        x_valid = map (\a -> a >= x0 && a <= x1) x
        y = takeWhile (>= y0) $ scanl1 (+) [dy - c | c <- [0..]]
        y_valid = map (\a -> a >= y0 && a <= y1) y
        valid = zipWith (&&) (x_valid ++ (cycle [last x_valid])) y_valid

part2 :: TargetArea -> Int
part2 ((x0, x1), (y0, y1)) = length options
  where oneShots = (x1 - x0 + 1) * (y1 - y0 + 1)
        vx_min = truncate $ maximum $ roots (1.0) (-1.0) (-2 * fromIntegral x0 :: Float)
        vy_max = -1 * y0 - 1
        options = [(vx,vy) | vx <- [vx_min..x1], vy <- [y0..vy_max], hitTarget ((x0, x1), (y0, y1)) (vx, vy)]

solve :: String -> (String, String)
solve input = (s1, s2)
  where entries = parse input
        s1 = show $ part1 entries -- 4095
        s2 = show $ part2 entries -- 3773
