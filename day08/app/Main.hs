module Main where

import Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    putStrLn $ "Part 1: " <> (show . sumMetadata . parseNodes . inputToInts $ input)
    putStrLn $
        "Part 2: "
            <> ( show
                    . nodeValue
                    . Just
                    . head
                    . parseNodes
                    . inputToInts
                    $ input
               )