module Main where

main :: IO ()
main = do
    input <- readFile "../../input/day01-in.txt"
    let measurements = map read $ lines input :: [Int]
    let result = depthIncreases measurements
    print result
    return ()

depthIncreases :: [Int] -> Int
depthIncreases measurements = foldl accumulateIncreases 0 mPairs
  where
    mPairs = measurements `zip` tail measurements

accumulateIncreases :: Int -> (Int, Int) -> Int
accumulateIncreases acc (pre, post)
    | post > pre = acc + 1
    | otherwise  = acc
