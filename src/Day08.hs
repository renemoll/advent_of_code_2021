module Day08 (solve) where

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
import Data.List

parse :: String -> ([String], [String])
parse xs = go xs
  where go :: String -> ([String], [String])
        go xs = (ys, tail zs)
          where s = words xs
                (ys, zs) = splitAt 10 s

-- part1 observations output = decoded output
part1 :: [([String], [String])] -> Int
part1 xs = sum $ map (\a -> go $ snd a) xs
  where go xs = ones + sevens + fours + eights
          where lengths = map length xs
                ones = length $ filter (==2) lengths
                sevens = length $ filter (==3) lengths
                fours = length $ filter (==4) lengths
                eights = length $ filter (==7) lengths

-- Remove characters from a string
removeItems :: String -> String -> String
removeItems [] ys = ys
removeItems (x:xs) ys = removeItems xs $ delete x ys

-- Invert the string
inverse :: String -> String
inverse xs = removeItems xs ['a'..'g']

-- Determine if each character is present in the string
contains :: String -> String -> Bool
contains (x:[]) ys = x `elem` ys
contains (x:xs) ys = if x `elem` ys then contains xs ys else False

decodeLength5 :: [String] -> String -> String -> (String, String, String)
decodeLength5 xs one six = (two, three, five)
  where three = xs !! (maybe 0 id $ True `elemIndex` (map (\a -> contains one a) xs))
        two = xs !! (maybe 0 id $ True `elemIndex` (map (\a -> (contains (inverse six) a) && not (contains one a)) xs))
        five = xs !! (maybe 0 id $ True `elemIndex` (map (\a -> (not (contains (inverse six) a)) && not (contains one a)) xs))

decodeLength6 :: [String] -> String -> String -> (String, String, String)
decodeLength6 xs one four = (zero, six, nine)
  where inv = map inverse xs
        zero = xs !! (maybe 0 id $ True `elemIndex` (map (\a -> (contains one a) && (contains (inverse a) four)) xs))
        six = xs !! (maybe 0 id $ True `elemIndex` (map (\a -> contains a one) inv))
        nine = xs !! (maybe 0 id $ True `elemIndex` (map (\a -> (contains four a) && (contains one a)) xs))

-- create a list where the n-th element represents the number n and contains the segments to display said number.
determineEncoding :: [String] -> [String]
determineEncoding xs = [zero, one, two, three, four, five, six, seven, eight, nine]
  where lengths = map length xs
        one = xs !! (maybe 0 id $ 2 `elemIndex` lengths)
        four = xs !! (maybe 0 id $ 4 `elemIndex` lengths)
        seven = xs !! (maybe 0 id $ 3 `elemIndex` lengths)
        eight = xs !! (maybe 0 id $ 7 `elemIndex` lengths)
        length6 = filter (\a -> (length a) == 6) xs
        (zero, six, nine) = decodeLength6 length6 one four
        length5 = filter (\a -> (length a) == 5) xs
        (two, three, five) = decodeLength5 length5 one six

-- decode dict code
list2num xs = go $ reverse xs
  where go (x:[]) = x
        go (x:xs) = (go xs) * 10 + x

decode :: [String] -> [String] -> Int
decode xs ys = list2num $ go xs ys
  where go xs [] = []
        go xs (y:ys) = (maybe 0 id $ True `elemIndex` (map (\a -> (contains y a) && (contains a y)) xs)) : go xs ys

part2 :: [([String], [String])] -> Int
part2 xs = sum $ map (\a -> go (fst a) (snd a)) xs
  where go xs ys = decode (determineEncoding xs) ys


solve :: String -> (Int, Int)
solve input = (s1, s2)
  where entries = map parse $ lines input
        s1 = part1 entries -- 247
        s2 = part2 entries -- 933305
