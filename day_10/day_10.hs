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

isClosingBracket :: Char -> Bool
isClosingBracket x = isJust $ translateCloser x

translateOpener :: Char -> Maybe Bracket
translateOpener x =
  case x of '(' -> Just RoundBracket
            '[' -> Just SquareBracket
            '{' -> Just CurlyBracket
            '<' -> Just AngleBracket
            otherwise -> Nothing

translateCloser :: Char -> Maybe Bracket
translateCloser x =
  case x of ')' -> Just RoundBracket
            ']' -> Just SquareBracket
            '}' -> Just CurlyBracket
            '>' -> Just AngleBracket
            otherwise -> Nothing

bracket2closer :: Bracket -> Char
bracket2closer x =
  case x of RoundBracket -> ')'
            SquareBracket -> ']'
            CurlyBracket -> '}'
            AngleBracket -> '>'

checkLine :: String -> Result
checkLine xs = go xs [] []
  where go :: String -> [Bracket] -> String -> Result
        go [] ys zs = Incomplete ys
        go (x:xs) ys zs = if isOpeningBracket x
                  then go xs (ys ++ [fromJust $ translateOpener x]) (zs ++ [x])
                  else
                    if (last ys) == (fromJust $ translateCloser x)
                    then go xs (init ys) (init zs)
                    else Illegal x

part1 :: [String] -> Int
part1 xs = sum $ map (points . checkLine) xs
  where points x =
          case x of Incomplete _ -> 0
                    Illegal y ->
                      case y of ')' -> 3
                                ']' -> 57
                                '}' -> 1197
                                '>' -> 25137

part2 :: [String] -> Int
part2 xs = result !! (length result `div` 2)
  where closers = filter (\a -> length a > 0) $ map (remainder . checkLine) xs
        remainder x =
          case x of Incomplete ys -> [bracket2closer y | y <- ys]
                    Illegal _ -> []
        result = sort [points2 c | c <- closers]
        points2 [] = 0
        points2 (c:cs) = p + 5 * (points2 cs)
          where p = case c of ')' -> 1
                              ']' -> 2
                              '}' -> 3
                              '>' -> 4

main :: IO ()
main = do
  entries <- lines <$> readFile "input.txt"

  print $ part1 entries -- 392367
  print $ part2 entries -- 2192104158
