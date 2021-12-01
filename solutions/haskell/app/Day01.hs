module Main where

import System.Environment

main :: IO ()
main = do
    argv <- getArgs
    let arg1 = read $ head argv :: Int
    let solver = multiplex arg1
    input <- readFile "../../input/day01-in.txt"
    let measurements = map read $ lines input :: [Int]
    let result = solver measurements
    print result
    return ()

multiplex arg1 = case arg1 of
  1 -> depthIncreases
  2 -> slidingWindowIncreases
  _ -> error "Santa says: \"there's nothing here, you may want to try on channels 1 or 2.\""


depthIncreases :: [Int] -> Int
depthIncreases measurements = foldl accumulateIncreases 0 mPairs
  where
    mPairs = measurements `zip` tail measurements

accumulateIncreases :: Int -> (Int, Int) -> Int
accumulateIncreases acc (pre, post)
    | post > pre = acc + 1
    | otherwise  = acc

slidingWindowIncreases :: [Int] -> Int
slidingWindowIncreases measurements = depthIncreases mWindowSums
  where
    mWindows = zip3 measurements (tail measurements) (drop 2 measurements)
    mWindowSums = map (\(a, b, c) -> a + b + c) mWindows
