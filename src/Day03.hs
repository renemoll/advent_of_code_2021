module Day03 (solve) where

import Data.List

parse :: String -> [Int]
parse = map (read . pure)

dominantSymbol :: [Int] -> Int
dominantSymbol xs = if sum xs * 2 >= length xs then 0 else 1

invert :: [Int] -> [Int]
invert = map $ (-) 1

bin2dec :: [Int] -> Int
bin2dec = foldl (\acc -> (+) (2 * acc)) 0

part1 :: [[Int]] -> Int
part1 = calc . transpose
  where calc xs = bin2dec gamma * bin2dec epsilon
          where gamma = map dominantSymbol xs
                epsilon = invert gamma

-- Reduce a set of vectors by applying `p` on each position (column) until 1 value remains.
select :: ([Int] -> Int) -> [[Int]] -> [Int]
select p = match 0
  where match pos ys
          | length ys == 1 = head ys
          | otherwise = match (pos + 1) (filtered p pos ys)

-- Filter values with a value on `pos` which satisfies `p`
filtered :: ([Int] -> Int) -> Int -> [[Int]] -> [[Int]]
filtered p pos xs = filter (\x -> x !! pos == d) xs
  where d = p (transpose xs !! pos)

mostCommon :: [Int] -> Int
mostCommon = dominantSymbol

leastCommon :: [Int] -> Int
leastCommon = (-) 1 . dominantSymbol

part2 :: [[Int]] -> Int
part2 xs = bin2dec oxi * bin2dec co2
  where oxi = select mostCommon xs
        co2 = select leastCommon xs

solve :: String -> (Int, Int)
solve input = (s1, s2)
  where numbers = map parse $ lines input
        s1 = part1 numbers -- 1131506
        s2 = part2 numbers -- 7863147
