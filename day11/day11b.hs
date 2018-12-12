module Main where

import           Control.Applicative            ( liftA2 )
import           Data.Vector.Unboxed            ( Vector
                                                , fromList
                                                , (!)
                                                )
import           System.Environment             ( getArgs )
import           System.IO                      ( hFlush
                                                , stdout
                                                )
import           Control.Parallel.Strategies    ( parMap
                                                , rdeepseq
                                                )

type Grid = Vector Int

type Cell = (Coord, Coord)

type Coord = Int
type SerialNum = Int

problemGridSize :: Int
problemGridSize = 300

main :: IO ()
main = do
  args <- getArgs
  if "test" `elem` args
    then tests
    else do
      putStr "Enter your puzzle input: "
      hFlush stdout
      serialNum <- read <$> getLine
      let grid = generateGrid problemGridSize serialNum
      print $ findVarSizePowerfulSquare grid

findVarSizePowerfulSquare :: Grid -> (Coord, Coord, Int)
findVarSizePowerfulSquare grid =
  fst
    $ foldr
        (\(size, ((x, y), power)) currMax@(_, maxPower) ->
          if power > maxPower then ((x, y, size), power) else currMax
        )
        ((0, 0, 0), 0)
    $ parMap rdeepseq
             (\size -> (size, findPowerfulSquare grid problemGridSize size))
             [1 .. problemGridSize]


findPowerfulSquare :: Grid -> Int -> Int -> (Cell, Int)
findPowerfulSquare grid gridSize squareSize =
  foldr
      (\cell@(x, y) currMax@(_, maxPower) ->
        let cellPower = sumSquare grid gridSize squareSize x y
        in  if cellPower > maxPower then (cell, cellPower) else currMax
      )
      ((0, 0), 0)
    $ combine [1 .. gridSize - squareSize + 1] [1 .. gridSize - squareSize + 1]

powerLevel :: Coord -> Coord -> SerialNum -> Int
powerLevel x y serialNum =
  let rackId = x + 10
  in  hundredsPlace (((rackId * y) + serialNum) * rackId) - 5

hundredsPlace :: (Integral a) => a -> a
hundredsPlace x =
  let rightTrimmed = x `quot` 100 in snd $ rightTrimmed `quotRem` 10

generateGrid :: Int -> SerialNum -> Grid
generateGrid size serialNum = fromList $ map
  (\i -> powerLevel ((i `rem` size) + 1) ((i `quot` size) + 1) serialNum)
  [0 .. size * size - 1]

getCellValue :: Grid -> Int -> Coord -> Coord -> Int
getCellValue grid size x y = grid ! ((x - 1) + (y - 1) * size)

sumSquare :: Grid -> Int -> Int -> Coord -> Coord -> Int
sumSquare grid gridSize squareSize x y = sum $ concatMap
  (\i -> map (getCellValue grid gridSize i) [y .. y + (squareSize - 1)])
  [x .. x + (squareSize - 1)]

combine :: [a] -> [b] -> [(a, b)]
combine = liftA2 (,)

-------------------------------------------------------
-------- Tests ----------------------------------------
-------------------------------------------------------

tests :: IO ()
tests = do
  testHundredsPlace
  testPowerLevel
  testGenerateGrid
  testGetCellValue
  testSumSquare
  testFindPowerfulSquare
  testFindVarSizePowerfulSquare

testEq :: (Eq a, Show a) => a -> a -> IO ()
testEq a b = putStrLn $ show a ++ " == " ++ show b ++ ": " ++ show (a == b)

testHundredsPlace :: IO ()
testHundredsPlace = do
  testEq (hundredsPlace 5431312) (3 :: Int)
  testEq (hundredsPlace 547312)  (3 :: Int)
  testEq (hundredsPlace 1234567) (5 :: Int)

testPowerLevel :: IO ()
testPowerLevel = do
  testEq (powerLevel 3 5 8)      4
  testEq (powerLevel 122 79 57)  (-5)
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
  testEq (findVarSizePowerfulSquare $ gridGenerator 18) (90 , 269, 16)
  testEq (findVarSizePowerfulSquare $ gridGenerator 42) (232, 251, 12)
