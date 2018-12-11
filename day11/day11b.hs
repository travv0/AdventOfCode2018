module Main where

import           Prelude                       as P
import           Control.Applicative           as A
import           Data.Sequence                 as S
import           System.IO                     as IO

type Grid = Seq (Seq Int)

type Cell = (Coord, Coord)

type Coord = Int
type SerialNum = Int

gridSize :: Int
gridSize = 300

main :: IO ()
main = do
  putStr "Enter your puzzle input: "
  hFlush stdout
  serialNum <- read <$> getLine
  print $ foldr
    (\size currMax@(_, maxPower) ->
      let ((x, y), squarePower) =
            findPowerfulSquare gridSize gridSize size size serialNum
      in  if squarePower > maxPower
            then ((x, y, size), squarePower)
            else currMax
    )
    ((0, 0, 0), Nothing)
    [1 .. gridSize]

findPowerfulSquare :: Int -> Int -> Int -> Int -> SerialNum -> (Cell, Maybe Int)
findPowerfulSquare gridWidth gridHeight squareWidth squareHeight serialNum =
  let grid = generateGrid gridWidth gridHeight serialNum
  in  foldr
          (\cell@(x, y) currMax@(_, maxPower) ->
            let cellPower = sumSquare grid squareWidth squareHeight x y
            in  if cellPower > maxPower then (cell, cellPower) else currMax
          )
          ((0, 0), Nothing)
        $ combine [1 .. gridWidth - squareWidth + 1]
                  [1 .. gridHeight - squareHeight + 1]

powerLevel :: Coord -> Coord -> SerialNum -> Int
powerLevel x y serialNum =
  let rackId = x + 10 in digit 3 (((rackId * y) + serialNum) * rackId) - 5

digit :: (Integral a, Read a) => Int -> a -> a
digit n x =
  let rightTrimmed = x `div` read ('1' : P.replicate (n - 1) '0')
  in  snd $ rightTrimmed `divMod` 10

generateGrid :: Int -> Int -> SerialNum -> Grid
generateGrid width height serialNum = S.fromList $ map
  (\x -> S.fromList $ map (\y -> powerLevel x y serialNum) [1 .. height])
  [1 .. width]

getCellValue :: Grid -> Coord -> Coord -> Maybe Int
getCellValue grid x y =
  if x
       <  1
       || y
       <  1
       || S.length grid
       <  x
       || case grid of
            (h :<| _) -> S.length h
            _         -> 0
       <  y
    then Nothing
    else S.lookup (x - 1) grid >>= S.lookup (y - 1)

sumSquare :: Grid -> Int -> Int -> Coord -> Coord -> Maybe Int
sumSquare grid width height x y =
  sumMaybe $ map (uncurry (getCellValue grid)) $ combine
    [x .. x + (width - 1)]
    [y .. y + (height - 1)]

sumMaybe :: Num a => [Maybe a] -> Maybe a
sumMaybe = fmap sum . sequence

combine :: [a] -> [b] -> [(a, b)]
combine = liftA2 (,)

-- gridWidth :: Grid -> Int
-- gridWidth = length

-- gridHeight :: Grid -> Int
-- gridHeight grid = if null grid then 0 else length $ head grid

-------------------------------------------------------
-------- Tests ----------------------------------------
-------------------------------------------------------

tests :: IO ()
tests = do
  testDigit
  testPowerLevel
  testGenerateGrid
  testGetCellValue
  testSumSquare
  testFindPowerfulSquare

testEq :: (Eq a, Show a) => a -> a -> IO ()
testEq a b = putStrLn $ show a ++ " == " ++ show b ++ ": " ++ show (a == b)

testDigit :: IO ()
testDigit = do
  testEq (digit 4 5431312) (1 :: Int)
  testEq (digit 5 547312)  (4 :: Int)
  testEq (digit 8 1234567) (0 :: Int)

testPowerLevel :: IO ()
testPowerLevel = do
  testEq (powerLevel 3 5 8)      4
  testEq (powerLevel 122 79 57)  (-5)
  testEq (powerLevel 217 196 39) 0
  testEq (powerLevel 101 153 71) 4

testGenerateGrid :: IO ()
testGenerateGrid = testEq (generateGrid 3 3 10) $ S.fromList $ map
  S.fromList
  [[-3, -2, -1], [-3, -1, 0], [-3, -1, 1]]

testGetCellValue :: IO ()
testGetCellValue = do
  let testGrid = generateGrid 3 5 10
  testEq (getCellValue testGrid 1 3) $ Just (-1)
  testEq (getCellValue testGrid 2 3) $ Just 0
  testEq (getCellValue testGrid 1 1) $ Just (-3)
  testEq (getCellValue testGrid 4 1) Nothing
  testEq (getCellValue testGrid 0 1) Nothing
  testEq (getCellValue testGrid 1 5) $ Just 2

testSumSquare :: IO ()
testSumSquare = do
  let gridGenerator = generateGrid 300 300
      sumGenerator n = sumSquare (gridGenerator n) 3 3
  testEq (sumGenerator 18 33 45) $ Just 29
  testEq (sumGenerator 42 21 61) $ Just 30
  testEq (sumGenerator 42 299 61) Nothing

testFindPowerfulSquare :: IO ()
testFindPowerfulSquare = do
  let ps = findPowerfulSquare 300 300 3 3
  testEq (ps 18) ((33, 45), Just 29)
  testEq (ps 42) ((21, 61), Just 30)
