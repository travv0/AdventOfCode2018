module Main where

import Lib

main :: IO ()
main = do
    (claims, ids) <- parseInput <$> readFile "input.txt"
    let (overlappingClaims, nonOverlappingId) = countOverlappingClaims claims ids
    putStrLn $ "Part 1: " <> show overlappingClaims
    putStrLn $ "Part 2: " <> show nonOverlappingId
