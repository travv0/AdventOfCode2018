module Main where

import Control.Lens (Ixed (ix), (^.))
import Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    let counts = countSleep $ lines input
        sleepiestGuard = mostMinutesId counts
        sleepiestMinute = mostMinutesTime (counts ^. ix sleepiestGuard)
    putStrLn $ "Part 1: " <> show (sleepiestGuard * sleepiestMinute)
