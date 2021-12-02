
nofIncrements :: [Int] -> Int
nofIncrements numbers = length $ filter (>0) $ zipWith (-) (tail numbers) numbers

sumThreemeasurements :: [Int] -> [Int]
sumThreemeasurements (x:y:z:zs) = [(x + y + z)] ++ sumThreemeasurements (y:z:zs)
sumThreemeasurements _ = []

nofIncrements3 :: [Int] -> Int
nofIncrements3 numbers = nofIncrements (sumThreemeasurements numbers)

main = do
  numbers <- map (read :: String -> Int) . lines <$> readFile "input.txt"
  print $ nofIncrements numbers
  print $ nofIncrements3 numbers
