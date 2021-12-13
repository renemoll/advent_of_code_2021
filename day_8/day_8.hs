--
-- 0: 6 segments
-- 1: 2 segments
-- 2: 5 segments
-- 3: 5 segments
-- 4: 4 segments
-- 5: 5 segments
-- 6: 6 segments
-- 7: 3 segments
-- 8: 7 segments
-- 9: 6 segments
--
-- 2 segments: 1
-- 3 segments: 7
-- 4 segments: 4
-- 5 segments: 2, 5
-- 6 segments: 0, 6, 9
-- 7 segments: 8
--

parse :: String -> ([String], [String])
parse xs = go xs
  where go :: String -> ([String], [String])
        go xs = (ys, tail zs)
          where s = words xs
                (ys, zs) = splitAt 10 s

--
-- Solution 1
--
-- For this first part, it is not required to properly
-- decode the segments. Simply counting the lengths
-- of the strings is enough to determine the unique
-- numbers and their occurrences.
--
part1 :: [([String], [String])] -> Int
part1 xs = sum $ map (\a -> go $ snd a) xs
  where go xs = ones + sevens + fours + eights
          where lengths = map length xs
                ones = length $ filter (==2) lengths
                sevens = length $ filter (==3) lengths
                fours = length $ filter (==4) lengths
                eights = length $ filter (==7) lengths

main :: IO ()
main = do
  entries <- map parse . lines <$> readFile "input.txt"

  print $ part1 entries -- 247
