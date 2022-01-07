import System.Environment

import Day01
import Day02
import Day03
import Day04
import Day05
import Day06
import Day07
import Day08
import Day09
import Day10
import Day11
import Day12
import Day13
import Day14
import Day15
import Day16
import Day17

days :: [String -> (String, String)]
days = [Day01.solve,
        Day03.solve,
        Day02.solve,
        Day04.solve,
        Day05.solve,
        Day06.solve,
        Day07.solve,
        Day08.solve,
        Day09.solve,
        Day10.solve,
        Day11.solve,
        Day12.solve,
        Day13.solve,
        Day14.solve,
        Day15.solve,
        Day16.solve,
        Day17.solve]

solveDay :: Int -> IO()
solveDay n = do
  input <- readFile $ concat ["./data/day_", show n, ".txt"]
  putStrLn $ "Solving day " ++ show n
  let (s1, s2) = (days !! (n - 1)) input
  putStrLn $ "- part 1: " ++ s1
  putStrLn $ "- part 2: " ++ s2

runAll :: IO()
runAll = do
  mapM_ solveDay [1..(length days)]

runLast :: IO()
runLast = solveDay $ length days

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["all"] -> runAll
    ["last"] -> runLast
    _ -> error "Unknown command line option, use `all`, `last` or a specific number"
