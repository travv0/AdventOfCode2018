import Lib

import Control.Monad (unless)
import GHC.Exts (fromList)
import System.Exit (exitFailure)

main :: IO ()
main = do
    testHundredsPlace
    testPowerLevel
    testGenerateGrid
    testGetCellValue
    testSumSquare
    testFindPowerfulSquare
    testFindVarSizePowerfulSquare

testEq :: (Eq a, Show a) => a -> a -> IO ()
testEq a b = do
    let check = a == b
    putStrLn $ show a ++ " == " ++ show b ++ ": " ++ show check
    unless check exitFailure

testHundredsPlace :: IO ()
testHundredsPlace = do
    testEq (hundredsPlace 5431312) (3 :: Int)
    testEq (hundredsPlace 547312) (3 :: Int)
    testEq (hundredsPlace 1234567) (5 :: Int)

testPowerLevel :: IO ()
testPowerLevel = do
    testEq (powerLevel 3 5 8) 4
    testEq (powerLevel 122 79 57) (-5)
    testEq (powerLevel 217 196 39) 0
    testEq (powerLevel 101 153 71) 4

testGenerateGrid :: IO ()
testGenerateGrid =
    testEq (generateGrid 3 10) $ fromList [-3, -3, -3, -2, -1, -1, -1, 0, 1]

testGetCellValue :: IO ()
testGetCellValue = do
    let testGrid = generateGrid 3 10
    testEq (getCellValue testGrid 3 1 3) (-1)
    testEq (getCellValue testGrid 3 2 3) 0
    testEq (getCellValue testGrid 3 1 1) (-3)

testSumSquare :: IO ()
testSumSquare = do
    let sumGenerator n = sumSquare (generateGrid 300 n) 300 3
    testEq (sumGenerator 18 33 45) 29
    testEq (sumGenerator 42 21 61) 30

testFindPowerfulSquare :: IO ()
testFindPowerfulSquare = do
    let ps n = findPowerfulSquare (generateGrid 300 n) 300 3
    testEq (ps 18) ((33, 45), 29)
    testEq (ps 42) ((21, 61), 30)

testFindVarSizePowerfulSquare :: IO ()
testFindVarSizePowerfulSquare = do
    let gridGenerator = generateGrid 300
    testEq (findVarSizePowerfulSquare (gridGenerator 18) 300) (90, 269, 16)
    testEq (findVarSizePowerfulSquare (gridGenerator 42) 300) (232, 251, 12)
