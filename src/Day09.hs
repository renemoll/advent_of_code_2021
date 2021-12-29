module Day09 (solve) where

parse :: String -> [Int]
parse = map (\x -> read [x])

getNeighbours :: [[Int]] -> Int -> Int -> (Int, [Int])
getNeighbours xs x y = (value, [l, r, u, d])
  where rows = length xs
        cols = length $ head xs
        value = (xs !! x) !! y
        l = if y > 0 then (xs !! x) !! (y - 1) else 9
        r = if (y + 1) < cols then (xs !! x) !! (y + 1) else 9
        u = if x > 0 then (xs !! (x - 1)) !! y else 9
        d = if (x + 1) < rows then (xs !! (x + 1)) !! y else 9

isLocalMinimum :: [[Int]] -> Int -> Int -> Bool
isLocalMinimum xs x y = minimum zs > z
  where (z,zs) = getNeighbours xs x y

part1 :: [[Int]] -> Int
part1 xs = sum $ [1 + x | x <- values]
  where rows = length xs
        cols = length $ head xs
        points = [(x,y) | x <- [0..(rows-1)], y <- [0..(cols-1)]]
        localMin = filter (uncurry (isLocalMinimum xs)) points
        values = [(xs !! fst a) !! snd a | a <- localMin]

part2 :: [[Int]] -> Int
part2 _ = 0

solve :: String -> (String, String)
solve input = (s1, s2)
  where entries = map parse $ lines input
        s1 = show $ part1 entries -- 603
        s2 = show $ part2 entries
