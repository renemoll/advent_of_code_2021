
parse :: String -> [Int]
parse xs = map (\x -> read [x]) xs

getNeighbours :: [[Int]] -> Int -> Int -> (Int, [Int])
getNeighbours xs x y = (value, [l, r, u, d])
  where rows = length xs
        cols = length $ xs !! 0
        value = (xs !! x) !! y
        l = if y > 0 then (xs !! x) !! (y - 1) else 9
        r = if (y + 1) < cols then (xs !! x) !! (y + 1) else 9
        u = if x > 0 then (xs !! (x - 1)) !! y else 9
        d = if (x + 1) < rows then (xs !! (x + 1)) !! y else 9

isLocalMinimum :: [[Int]] -> Int -> Int -> Bool
isLocalMinimum xs x y = (minimum zs) > z
  where (z,zs) = getNeighbours xs x y


part1 xs = sum $ [1 + x | x <- values]
  where rows = length xs
        cols = length $ xs !! 0
        points = [(x,y) | x <- [0..(rows-1)], y <- [0..(cols-1)]]
        localMin = filter (\a -> isLocalMinimum xs (fst a) (snd a)) points
        values = [(xs !! (fst a)) !! (snd a) | a <- localMin]


main :: IO ()
main = do
  entries <- map parse . lines <$> readFile "input.txt"

  print $ part1 entries -- 603
