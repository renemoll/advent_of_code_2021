module Day05 (solve) where

import Data.List
import Data.List.Split

data Coordinate = Coordinate Int Int
                  deriving (Read, Show, Eq, Ord)

data Line = Line { start :: Coordinate,
                   end :: Coordinate }
            deriving (Show)

parse :: String -> Line
parse x = Line (Coordinate x0 y0) (Coordinate x1 y1)
  where [a, b] = splitOn " -> " x
        [x0, y0] = map read $ splitOn "," a
        [x1, y1] = map read $ splitOn "," b

isStraightLine :: Line -> Bool
isStraightLine l = x0 == x1 || y0 == y1
  where (Coordinate x0 y0) = start l
        (Coordinate x1 y1) = end l

-- Generate a list of numbers from x to y (both positive as negative)
generateRange :: Int -> Int -> [Int]
generateRange x y = if x <= y
                    then enumFromTo x y
                    else reverse $ enumFromTo y x

-- Translate a line into a set of coordinates forming said line
rasterize :: Line -> [Coordinate]
rasterize (Line (Coordinate x0 y0) (Coordinate x1 y1)) = map (uncurry Coordinate) range
  where xRange = generateRange x0 x1
        yRange = generateRange y0 y1
        limit = max (length xRange) (length yRange)
        range = take limit $ zip (cycle xRange) (cycle yRange)

countOverlap :: [Line] -> Int
countOverlap xs = length $ filter (>1) lengths
  where points = map rasterize xs
        matched = group $ sort $ concat points
        lengths = map length matched

part1 :: [Line] -> Int
part1 xs = countOverlap validLines
  where validLines = filter isStraightLine xs

part2 :: [Line] -> Int
part2 = countOverlap

solve :: String -> (String, String)
solve input = (s1, s2)
  where entries = map parse $ lines input
        s1 = show $ part1 entries -- 5092
        s2 = show $ part2 entries -- 20484
