module Main where

import Lib

main :: IO ()
main = do
    claims <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " <> show (countOverlappingClaims claims)
