module Main where

import Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 1: " <> show (length $ reduce input)
    putStrLn $ "Part 2: " <> show (findShortestReduction input)