import Data.List

dominantSymbol :: String -> Int
dominantSymbol xs = let z = length (filter (=='0') xs)
                        l = length xs
                    in if (2 * z) > l
                       then 0
                       else 1

invert :: [Int] -> [Int]
invert xs = [if x == 1 then 0 else 1 | x <- xs]

bin2dec :: [Int] -> Int
bin2dec = foldl (\acc -> (+) (2 * acc)) 0

part1 xs = bin2dec gamma * bin2dec epsilon
  where t = transpose xs
        gamma = map dominantSymbol t
        epsilon = invert gamma

main :: IO ()
main = do
  entries <- lines <$> readFile "/root/input.txt"
  print $ part1 entries
