module Day01 (solve) where

parse :: String -> Int
parse = read :: String -> Int

-- Find the number of times a number increases w.r.t. the previous number.
nofIncrements :: [Int] -> Int
nofIncrements numbers = length $ filter (>0) $ zipWith (-) (tail numbers) numbers

sumThreeMeasurements :: [Int] -> [Int]
sumThreeMeasurements (x:y:z:zs) = (x + y + z) : sumThreeMeasurements (y:z:zs)
sumThreeMeasurements _ = []

-- Find the number of times a moving sum of 3 numbers increases w.r.t. the previous sum of 3 numbers.
nofIncrements3 :: [Int] -> Int
nofIncrements3 numbers = nofIncrements (sumThreeMeasurements numbers)

solve :: String -> (Int, Int)
solve input = (s1, s2)
  where numbers = map parse $ lines input
        s1 = nofIncrements numbers -- 1301
        s2 = nofIncrements3 numbers -- 1346
