module Day13 (solve) where

import Data.List
import Data.List.Split
import Data.Ord

parse :: String -> ([(Int, Int)], [(String, Int)])
parse xs = (map parseDots (lines dots), map parseFolds (lines folds))
  where [dots, folds] = splitOn "\n\n" xs
        parseDots :: String -> (Int, Int)
        parseDots xs = toTuple $ map (\a -> read a :: Int) $ splitOn "," xs
        toTuple :: [Int] -> (Int, Int)
        toTuple [x,y] = (x,y)
        parseFolds :: String -> (String, Int)
        parseFolds xs = let
                          [a,b] = splitOn "=" xs
                        in (drop 11 a, read b)

foldX :: Int -> [(Int, Int)] -> [(Int, Int)]
foldX n xs = pre ++ post
  where pre = [x | x <- xs, fst x < n]
        post = [(2*n-x, y) | (x, y) <- xs, x > n]

foldY :: Int -> [(Int, Int)] -> [(Int, Int)]
foldY n xs = pre ++ post
  where pre = [x | x <- xs, snd x < n]
        post = [(x, 2*n-y) | (x, y) <- xs, y > n]

unique :: [(Int, Int)] -> [(Int, Int)]
unique = map head . group . sort

applyFold :: (String, Int) -> [(Int, Int)] -> [(Int, Int)]
applyFold ("y", n) xs = foldY n xs
applyFold ("x", n) xs = foldX n xs

part1 :: [(String, Int)] -> [(Int, Int)] -> Int
part1 folds entries = length $ unique $ applyFold (head folds) entries

printCode :: [(Int, Int)] -> String
printCode xs = unlines [[if (x,y) `elem` xs then '#' else '.' | x <- [0..maxX]] | y <- [0..maxY]]
  where maxX = maximum $ map fst xs
        maxY = maximum $ map snd xs

part2 :: [(String, Int)] -> [(Int, Int)] -> String -- [(Int, Int)]
part2 folds entries = printCode $ go folds entries
  where go :: [(String, Int)] -> [(Int, Int)] -> [(Int, Int)]
        go [] ys = ys
        go (x:xs) ys = go xs $ unique $ applyFold x ys

solve :: String -> (String, String)
solve input = (s1, s2)
  where (entries, folds) = parse input
        s1 = show $ part1 folds entries -- 693
        s2 = part2 folds entries -- UCLZRAZUs
