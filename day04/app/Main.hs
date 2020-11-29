module Main where

import Data.IntMap ((!))
import Lib

main :: IO ()
main = do
    input <- readFile "input.txt"
    let counts = countSleep $ lines input
        sleepiestGuard = mostMinutesId counts
        (sleepiestMinute, _) = mostMinutesTime (counts ! sleepiestGuard)
        (sleepiestIdByMinute, sleepiestMinuteByMinute) = mostSameMinuteId counts
    putStrLn $ "Part 1: " <> show (sleepiestGuard * sleepiestMinute)
    putStrLn $ "Part 2: " <> show (sleepiestIdByMinute * sleepiestMinuteByMinute)
