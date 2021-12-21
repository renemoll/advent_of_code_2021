module Day08 (solve) where

import Data.List
import Data.Maybe

--
-- Decoding:
-- * Based on length:
--    * 1: length == 2
--    * 4: length == 4
--    * 7: length == 3
--    * 8: length == 7
-- * Based on length == 6 and deductions:
--    * 0: contains 1 and its inverse (d) is in 4
--    * 6: its inverse is in 1
--    * 9: contains 4
-- * Based on length == 5 and deductions:
--    * 2: contains the inverse of 6 (c)
--    * 3: only one in this set which contains 1
--    * 5: does not the inverse of 6 (c)
--

parse :: String -> ([String], [String])
parse = go
  where go :: String -> ([String], [String])
        go ys = let (a, b) = splitAt 10 $ words ys in (a, tail b)

-- Total number of observations of ones, sevens, fours and eights
part1 :: [([String], [String])] -> Int
part1 xs = sum $ map (go . snd) xs
  where go ys = ones + sevens + fours + eights
          where lengths = map length ys
                ones = length $ filter (==2) lengths
                sevens = length $ filter (==3) lengths
                fours = length $ filter (==4) lengths
                eights = length $ filter (==7) lengths

-- Remove characters from a string
removeItems :: String -> String -> String
removeItems xs ys = foldr delete ys xs
--removeItems xs ys = foldl (flip delete) ys xs

-- Invert the string
inverse :: String -> String
inverse xs = removeItems xs ['a'..'g']

-- Determine if each character is present in the string
contains :: String -> String -> Bool
contains [] _ = True
contains (x:xs) ys = (x `elem` ys) && contains xs ys

decodeLength5 :: [String] -> String -> String -> (String, String, String)
decodeLength5 xs one six = (two, three, five)
  where three = xs !! fromMaybe 0 (True `elemIndex` map (contains one) xs)
        two = xs !! fromMaybe 0 (True `elemIndex` map (\a -> contains (inverse six) a && not (contains one a)) xs)
        five = xs !! fromMaybe 0 (True `elemIndex` map (\a -> not (contains (inverse six) a) && not (contains one a)) xs)

decodeLength6 :: [String] -> String -> String -> (String, String, String)
decodeLength6 xs one four = (zero, six, nine)
  where inv = map inverse xs
        zero = xs !! fromMaybe 0 (True `elemIndex` map (\a -> contains one a && contains (inverse a) four) xs)
        six = xs !! fromMaybe 0 (True `elemIndex` map (`contains` one) inv)
        nine = xs !! fromMaybe 0 (True `elemIndex` map (\a -> contains four a && contains one a) xs)

-- create a list where the n-th element represents the number n and contains the segments to display said number.
determineEncoding :: [String] -> [String]
determineEncoding xs = [zero, one, two, three, four, five, six, seven, eight, nine]
  where lengths = map length xs
        one = xs !! fromMaybe 0 (2 `elemIndex` lengths)
        four = xs !! fromMaybe 0 (4 `elemIndex` lengths)
        seven = xs !! fromMaybe 0 (3 `elemIndex` lengths)
        eight = xs !! fromMaybe 0 (7 `elemIndex` lengths)
        length6 = filter (\a -> length a == 6) xs
        (zero, six, nine) = decodeLength6 length6 one four
        length5 = filter (\a -> length a == 5) xs
        (two, three, five) = decodeLength5 length5 one six

-- decode dict code
list2num :: Num a => [a] -> a
list2num xs = go $ reverse xs
  where go [] = 0
        go [y] = y
        go (y:ys) = go ys * 10 + y

decode :: [String] -> [String] -> Int
decode xs ys = list2num $ go xs ys
  where go _ [] = []
        go zs (b:bs) = fromMaybe 0 (True `elemIndex` map (\a -> contains b a && contains a b) zs) : go zs bs

part2 :: [([String], [String])] -> Int
part2 xs = sum $ map (uncurry go) xs
  where go ys = decode (determineEncoding ys)

solve :: String -> (String, String)
solve input = (s1, s2)
  where entries = map parse $ lines input
        s1 = show $ part1 entries -- 247
        s2 = show $ part2 entries -- 933305
