module Main where

import           Data.List

type Marble = Int

main :: IO ()
main = print "HI"

calculateHighScore :: Int -> Marble -> Int
calculateHighScore playerCount lastMarble = 0

placeMarble :: [Marble] -> Marble -> Marble -> [Marble]
placeMarble marbles marble currentMarble =
  let (front, back) = case elemIndex currentMarble marbles of
        Just i  -> splitAt (clockwise (length marbles) i 1 + 1) marbles
        Nothing -> ([], marbles)
  in  front ++ [marble] ++ back

counterClockwise :: Int -> Int -> Int -> Int
counterClockwise count i distance = (i + count - distance) `mod` count

clockwise :: Int -> Int -> Int -> Int
clockwise count i distance = (i + distance) `mod` count

-------------------------------------------------------
-------- Tests ----------------------------------------
-------------------------------------------------------

tests :: IO ()
tests = do
  testCalculateHighScore
  testPlaceMarble
  testCounterClockwise
  testClockwise

testEq :: (Eq a, Show a) => a -> a -> IO ()
testEq a b = putStrLn $ show a ++ " == " ++ show b ++ ": " ++ show (a == b)

testCalculateHighScore :: IO ()
testCalculateHighScore = do
  testEq (calculateHighScore 9 25)    32
  testEq (calculateHighScore 10 1618) 8317
  testEq (calculateHighScore 13 7999) 146474
  testEq (calculateHighScore 17 1104) 2764
  testEq (calculateHighScore 21 6111) 54718
  testEq (calculateHighScore 30 5807) 37305

testPlaceMarble :: IO ()
testPlaceMarble = do
  testEq (placeMarble [0] 1 0)             [0, 1]
  testEq (placeMarble [0, 1] 2 1)          [0, 2, 1]
  testEq (placeMarble [0, 2, 1] 3 2)       [0, 2, 1, 3]
  testEq (placeMarble [0, 2, 1, 3] 4 3)    [0, 4, 2, 1, 3]
  testEq (placeMarble [0, 4, 2, 1, 3] 5 4) [0, 4, 2, 5, 1, 3]

testCounterClockwise :: IO ()
testCounterClockwise = do
  testEq (counterClockwise 4 3 1) 2
  testEq (counterClockwise 4 3 2) 1
  testEq (counterClockwise 4 3 3) 0
  testEq (counterClockwise 4 3 4) 3

testClockwise :: IO ()
testClockwise = do
  testEq (clockwise 4 3 1) 0
  testEq (clockwise 4 3 2) 1
  testEq (clockwise 4 3 3) 2
  testEq (clockwise 4 3 4) 3
