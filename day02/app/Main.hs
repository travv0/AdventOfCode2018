module Main where

import Lib

main :: IO ()
main = do
    ls <- lines <$> readFile "input.txt"
    putStrLn $ "Part 1: " <> show (generateChecksum ls)
    putStrLn $ "Part 2: " <> findMatch ls