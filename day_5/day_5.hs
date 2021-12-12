import Data.List
import Data.List.Split


data Coordinate = Coordinate Int Int
                  deriving (Read, Show, Eq, Ord)

data Line = Line { start :: Coordinate,
                   end :: Coordinate }
            deriving (Show)

parse :: String -> [Line]
parse = map go . lines
  where go :: String -> Line
        go x = Line (Coordinate x0 y0) (Coordinate x1 y1)
          where [a, b] = splitOn " -> " x
                [x0, y0] = map read $ splitOn "," a
                [x1, y1] = map read $ splitOn "," b

isStraightLine :: Line -> Bool
isStraightLine l = x0 == x1 || y0 == y1
  where (Coordinate x0 y0) = start l
        (Coordinate x1 y1) = end l

-- Generate a list of numbers from x to y (both positive as negative)
generateRange x y = if x <= y
                    then enumFromTo x y
                    else reverse $ enumFromTo y x

-- Translate a line into a set of coordinates forming said line
rasterize :: Line -> [Coordinate]
rasterize (Line (Coordinate x0 y0) (Coordinate x1 y1)) = map (\a -> Coordinate (fst a) (snd a)) range
  where xRange = generateRange x0 x1
        yRange = generateRange y0 y1
        limit = max (length xRange) (length yRange)
        range = take limit $ zip (cycle xRange) (cycle yRange)

part1 xs = length $ filter (>1) lengths
  where validLines = filter isStraightLine xs
        points = map rasterize validLines
        matched = group $ sort $ concat points
        lengths = map length matched

part2 xs = length $ filter (>1) lengths
  where points = map rasterize xs
        matched = group $ sort $ concat points
        lengths = map length matched

main :: IO ()
main = do
  entries <- parse <$> readFile "input.txt"

  print $ part1 entries -- 5092
  print $ part2 entries -- 20484
