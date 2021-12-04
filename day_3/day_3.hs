import Data.List

dominantSymbol :: [Int] -> Int
dominantSymbol xs = if sum xs * 2 >= (length xs) then 0 else 1

invert :: [Int] -> [Int]
invert = map $ (-) 1

bin2dec :: [Int] -> Int
bin2dec = foldl (\acc -> (+) (2 * acc)) 0

part1 :: [[Int]] -> Int
part1 = calc . transpose
  where calc xs = bin2dec gamma * bin2dec epsilon
          where gamma = map dominantSymbol xs
                epsilon = invert gamma

select :: ([Int] -> Int) -> [[Int]] -> [Int]
select pred xs = match 0 xs
  where
    match pos xs
          | length xs == 1 = head xs
          | otherwise = match (pos + 1) (filtered pred pos xs)

filtered :: ([Int] -> Int) -> Int -> [[Int]] -> [[Int]]
filtered pred pos xs = filter (\x -> x !! pos == d) xs
  where d = pred (transpose xs !! pos)

mostCommon = dominantSymbol
leastCommon = (-) 1 . dominantSymbol

part2 :: [[Int]] -> Int
part2 = calc
  where calc xs = bin2dec oxi * bin2dec co2
          where oxi = select mostCommon xs
                co2 = select leastCommon xs

parse :: String -> [[Int]]
parse = map lineToBinary . lines
  where lineToBinary = map (read . pure)

main :: IO ()
main = do
  entries <- parse <$> readFile "input.txt"
  print $ part1 entries
  print $ part2 entries
