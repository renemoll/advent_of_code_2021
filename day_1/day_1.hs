
nof_increments :: [Int] -> Int
nof_increments numbers = length $ filter (>0) [x - y | (x,y) <- (zip (tail numbers) numbers)]

sum_3_measurements :: [Int] -> [Int]
sum_3_measurements [] = []
sum_3_measurements (x:[]) = []
sum_3_measurements (x:y:[]) = []
sum_3_measurements (x:y:z:xs) = [(x+y+z)] ++ sum_3_measurements ([y, z] ++ xs)

nof_increments3 :: [Int] -> Int
nof_increments3 numbers = nof_increments (sum_3_measurements numbers)

main = do
  numbers <- map (read :: String -> Int) . lines <$> readFile "input.txt"
  print $ nof_increments numbers
  print $ nof_increments3 numbers
