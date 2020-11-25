module Main where

import Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    let (pots, notes) = parseInput input
        (offset1, endPots1) = runGenerations notes pots 20
        (offset2, endPots2) = runGenerations notes pots 50000000000
    putStrLn $ "Part 1: " <> show (sumPlantNums offset1 endPots1)
    putStrLn $ "Part 2: " <> show (sumPlantNums offset2 endPots2)