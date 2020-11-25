module Main where

import Lib

main :: IO ()
main = do
    let input = 9995
    let gridSize = 300
    let grid = generateGrid gridSize input
    putStrLn $ "Part 1: " <> (format2dCoords . fst) (findPowerfulSquare grid gridSize 3)
    putStrLn $ "Part 2: " <> format3dCoords (findVarSizePowerfulSquare grid gridSize)