module Main where

import Lib

main :: IO ()
main = do
    ls <- lines <$> readFile "input.txt"
    putStrLn $ "Part 1: " <> (show . findMaxClosest . parseCoords $ ls)
    putStrLn $
        "Part 2: "
            <> ( show
                    . length
                    . notTooFar maximumDistance
                    . parseCoords
                    $ ls
               )