module Main where

import Lib

main :: IO ()
main = do
    (playerCount, lastMarble) <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " <> show (calculateHighScore playerCount lastMarble)
    putStrLn $ "Part 2: " <> show (calculateHighScore playerCount (lastMarble * 100))