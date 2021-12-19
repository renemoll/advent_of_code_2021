module Day01 (solve) where

nofIncrements :: [Int] -> Int
nofIncrements numbers = length $ filter (>0) $ zipWith (-) (tail numbers) numbers

sumThreemeasurements :: [Int] -> [Int]
sumThreemeasurements (x:y:z:zs) = [(x + y + z)] ++ sumThreemeasurements (y:z:zs)
sumThreemeasurements _ = []

nofIncrements3 :: [Int] -> Int
nofIncrements3 numbers = nofIncrements (sumThreemeasurements numbers)

solve :: String -> (Int, Int)
solve input = (s1, s2)
  where numbers = map (read :: String -> Int) $ lines input
        s1 = nofIncrements numbers
        s2 = nofIncrements3 numbers
