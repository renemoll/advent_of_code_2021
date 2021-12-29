module Day14 (solve) where

import Data.List
import Data.List.Split
import qualified Data.Map as Map

-- idea: predetermine the output of a step
parse :: String -> (String, Map.Map String String)
parse xs = (template, Map.fromList $ map parseRules (lines rules))
  where [template, rules] = splitOn "\n\n" xs
        parseRules :: String -> (String, String)
        parseRules xs = toTuple $ splitOn " -> " xs
        toTuple :: [a] -> (a, a)
        toTuple [x,y] = (x,y)

--
-- Part 1: naive solution
--

step :: Map.Map String String -> String -> String
step rules start = go start
  where go :: String -> String
        go [] = ""
        go (x:[]) = [x]
        go (x:y:ys) = [x] ++ (rules Map.! (x:[y])) ++ (go ([y] ++ ys))

part1 :: Map.Map String String -> Int -> String -> Int
part1 rules n start = maximum counts - minimum counts
  where go :: Map.Map String String -> Int -> String -> String
        go rules 0 xs = xs
        go rules n xs = go rules (n - 1) $ step rules xs
        result = go rules n start
        counts = map length (group $ sort result)

--
-- Part 2: optimized solution
--
-- Instead of keeping the complete string, I now track a finite number of combinations
-- and their occourance. Each step splits each combination and adds to the previously
-- recordded amount of occourances.
--
-- Note: this method does internally count each element twice, except for the first
-- and last element of the template.
--

part2 :: Map.Map String String -> Int -> String -> Int
part2 rules n start = maximum counts_3 - minimum counts_3
  where go :: Map.Map String String -> Int -> Map.Map String Int -> Map.Map String Int
        go rules 0 xs = xs
        go rules n xs = go rules (n - 1) $ step2 rules xs
        result = go rules n (prep2 start)
        counts = count2 result
        counts_1 = Map.insertWith (+) (head start) 1 counts
        counts_2 = Map.insertWith (+) (last start) 1 counts_1
        counts_3 = Map.elems counts_2

prep2 :: String -> Map.Map String Int
prep2 xs = Map.fromListWith (+) $ zip combi (repeat 1)
  where combi = takeWhile ((==2) . length) $ map (take 2) (tails xs)

-- take the actual count into account :)
step2 :: Map.Map String String -> Map.Map String Int -> Map.Map String Int
step2 rules input = Map.fromListWith (+) $ result
  where go :: String -> [(String, Int)]
        go key@(a:b) = let c = rules Map.! key in zip [a:c, c ++ b] (repeat (input Map.! key))
        result = concatMap go (Map.keys input)

count2 :: Map.Map String Int -> Map.Map Char Int
count2 input = Map.map (flip div 2) $ Map.fromListWith (+) $ result
  where go :: String -> [(Char, Int)]
        go key@(a:b:_) = let n = input Map.! key in [(a, n), (b, n)]
        result = concatMap go (Map.keys input)

solve :: String -> (String, String)
solve input = (s1, s2)
  where (template, rules) = parse input
        s1 = show $ part1 rules 10 template -- 2899
        s2 = show $ part2 rules 40 template -- 3528317079545
