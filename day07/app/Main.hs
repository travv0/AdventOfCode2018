module Main where

import Lib

main :: IO ()
main = do
    graph <- graphFromString <$> readFile "input.txt"
    putStrLn $ "Part 1: " <> naiveTopSort graph
    putStrLn $ "Part 2: " <> show (timedParallelTopSort graph 5 60)