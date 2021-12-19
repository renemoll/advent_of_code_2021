module Day04 (solve) where

import Data.List
import Data.List.Split
import Data.Maybe

data BingoCell = Number { bingoInt :: Int }
               | Marked
                 deriving (Show, Eq)

type BingoCard = [BingoCell]

-- Parse the input format into a list on numbers and bingo cards.
parse :: [String] -> ([Int], [BingoCard])
parse input = (numbers, cards)
  where numbers = [read x :: Int | x <- (splitOn "," $ input !! 0)]
        cards = [[Number (read x :: Int) | x <- concat $ map words (lines y)] | y <- (tail input)]

-- Mark a number on a card (if it is there) and return the marked card
mark :: Int -> BingoCard -> BingoCard
mark x card =
  case n of
    Just i -> let (pre, (_:post)) = splitAt i card in pre ++ [Marked] ++ post
    Nothing -> card
  where n = (Number x) `elemIndex` card

-- Get the row-space of our BingoCard (matrix)
card2rows :: BingoCard -> [[BingoCell]]
card2rows card = init $ go card
  where go :: [BingoCell] -> [[BingoCell]]
        go [] = [[]]
        go xs = [row] ++ rows
          where row = take 5 xs
                rows = go $ drop 5 xs

-- Get the col-space of our BingoCard (matrix)
card2cols :: BingoCard -> [[BingoCell]]
card2cols = transpose . card2rows

-- Check if a card has bingo
hasBingo :: BingoCard -> Bool
hasBingo card = any (\a -> a == 5) ys
  where xs = (card2rows card) ++ (card2cols card)
        ys = [length $ filter (==Marked) x | x <- xs]

-- Sum all the remaining numbers on a card
sumCard :: BingoCard -> Int
sumCard cells = sum $ [cell2int x | x <- cells]
  where cell2int c = case c of
                        Marked -> 0
                        Number y -> y

-- Solve part 1: find the first winning card
part1 :: [BingoCard] -> [Int] -> Maybe Int
part1 cards [] = Nothing
part1 cards (x:xs) = if (any hasBingo ys) then Just (x * acc) else part1 ys xs
  where ys = [mark x card | card <- cards]
        bingo = [hasBingo y | y <- ys]
        index = maybe 0 id (True `elemIndex` bingo)
        acc = sumCard (ys !! index)

-- Solve part 2: find the last winning card
part2 :: [BingoCard] -> [Int] -> Maybe Int
part2 cards [] = Nothing
part2 cards (x:xs) = if (all hasBingo ys) then Just (x * acc) else part2 ys xs
  where ys = [mark x card | card <- cards]
        bingo = [hasBingo y | y <- ys]
        diff = zipWith (==) bingo [hasBingo y | y <- cards]
        index = maybe 0 id (False `elemIndex` diff)
        acc = sumCard (ys !! index)

solve :: String -> (Int, Int)
solve input = (s1, s2)
  where (numbers, cards) = parse $ splitOn "\n\n" input
        s1 = fromJust $ part1 cards numbers -- 82440
        s2 = fromJust $ part2 cards numbers -- 20774
