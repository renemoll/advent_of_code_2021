import Data.List
import Data.List.Split

mark :: Int -> [Int] -> [Int]
-- Find `number` in `card` and replace it with -1
mark number card =
  case n of
    Just i -> let (pre, (_:post)) = splitAt i card in pre ++ [-1] ++ post
    Nothing -> card
  where n = number `elemIndex` card

hasBingo :: [Int] -> Bool
hasBingo card = any (\a -> a == 5) ys
  where xs = (cardRows card) ++ (cardCols card)
        ys = [length $ filter (==(-1)) x | x <- xs]

cardRows :: [Int] -> [[Int]]
cardRows card = init $ go card
  where go :: [Int] -> [[Int]]
        go [] = [[]]
        go xs = [row] ++ rows
          where row = take 5 xs
                rows = go $ drop 5 xs

cardCols :: [Int] -> [[Int]]
cardCols card = init $ go [0..4]
  where go [] = [[]]
        go (x:xs) = [col] ++ cols
          where col = map fst $ filter (\a -> snd a == x) $ zip card $ cycle [0..4]
                cols = go xs

part1 cards [] = -1
part1 cards (x:xs) = if check then acc * x else part1 ys xs
  where ys = [mark x card | card <- cards]
        bingo = [hasBingo y | y <- ys] -- could use `any hasBingo cards` to get a single anwser
        check = foldr (||) False [hasBingo x | x <- ys]
        index = maybe 0 id (True `elemIndex` bingo)
        acc = sum $ filter (>0) (ys !! index)

part2 cards [] = -1
part2 cards (x:xs) = if check then acc*x else part2 ys xs
  where ys = [mark x card | card <- cards]
        bingo = [hasBingo y | y <- ys] -- could use `any hasBingo cards` to get a single anwser
        check = foldr (&&) True [hasBingo x | x <- ys]
        prev = [hasBingo y | y <- cards]
        diff = zipWith (==) bingo prev
        index = maybe 0 id (False `elemIndex` diff)
        acc = sum $ filter (>0) (ys !! index)

main :: IO ()
main = do
  entries <- splitOn "\n\n" <$> readFile "input.txt"
  let numbers = [read x :: Int | x <- (splitOn "," $ entries !! 0)]
  let boards = tail entries
  let cards = [[read x :: Int | x <- concat $ map words (lines y)] | y <- boards]

  print $ part1 cards numbers -- 82440
  print $ part2 cards numbers -- 20774
