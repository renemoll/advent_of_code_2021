module Day10 (solve) where

import Data.Maybe
import Data.List

data Bracket = RoundBracket
              | SquareBracket
              | CurlyBracket
              | AngleBracket
              deriving (Show, Eq)

data Result = Incomplete [Bracket]
              | Illegal Char
              deriving (Show)

isOpeningBracket :: Char -> Bool
isOpeningBracket x = isJust $ translateOpener x

translateOpener :: Char -> Maybe Bracket
translateOpener x =
  case x of '(' -> Just RoundBracket
            '[' -> Just SquareBracket
            '{' -> Just CurlyBracket
            '<' -> Just AngleBracket
            _ -> Nothing

translateCloser :: Char -> Maybe Bracket
translateCloser x =
  case x of ')' -> Just RoundBracket
            ']' -> Just SquareBracket
            '}' -> Just CurlyBracket
            '>' -> Just AngleBracket
            _ -> Nothing

bracket2closer :: Bracket -> Char
bracket2closer x =
  case x of RoundBracket -> ')'
            SquareBracket -> ']'
            CurlyBracket -> '}'
            AngleBracket -> '>'

checkLine :: String -> Result
checkLine xs = go xs [] []
  where go :: String -> [Bracket] -> String -> Result
        go [] zs _ = Incomplete zs
        go (y:ys) zs as
          | isOpeningBracket y = go ys (zs ++ [fromJust $ translateOpener y]) (as ++ [y])
          | last zs == fromJust (translateCloser y) = go ys (init zs) (init as)
          | otherwise = Illegal y

part1 :: [String] -> Int
part1 xs = sum $ map (points . checkLine) xs
  where points x =
          case x of Incomplete _ -> 0
                    Illegal y ->
                      case y of ')' -> 3
                                ']' -> 57
                                '}' -> 1197
                                '>' -> 25137
                                _ -> 0

part2 :: [String] -> Int
part2 xs = result !! (length result `div` 2)
  where closers = filter (not . null) $ map (remainder . checkLine) xs
        remainder x =
          case x of Incomplete ys -> [bracket2closer y | y <- ys]
                    Illegal _ -> []
        result = sort [points2 c | c <- closers]
        points2 [] = 0
        points2 (c:cs) = p + 5 * points2 cs
          where p = case c of ')' -> 1
                              ']' -> 2
                              '}' -> 3
                              '>' -> 4
                              _ -> 0

solve :: String -> (Int, Int)
solve input = (s1, s2)
  where entries = lines input
        s1 = part1 entries -- 392367
        s2 = part2 entries -- 2192104158
