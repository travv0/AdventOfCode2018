module Main where

import           Prelude                       as P
import           Data.Maybe
import           Data.Foldable
import           Data.Sequence                 as S
import           Deque                         as D

type Marble = Int

main :: IO ()
main = do
  (playerCount, lastMarble) <- parseInput <$> getLine
  print $ calculateHighScore playerCount $ lastMarble * 100

parseInput :: String -> (Int, Int)
parseInput s = let w = words s in (read (P.head w), read (w !! 6))

calculateHighScore :: Int -> Marble -> Int
calculateHighScore playerCount lastMarble =
  let scores = S.replicate playerCount 0
  in  maximum $ calculateHighScore' 0 scores (D.fromList [0]) 1
 where
  calculateHighScore' :: Int -> Seq Int -> Deque Marble -> Marble -> Seq Int
  calculateHighScore' playerNum scores marbles newMarble
    | newMarble > lastMarble
    = scores
    | newMarble `mod` 23 == 0
    = let newMarbles = removeMarble marbles
      in  calculateHighScore' ((playerNum + 1) `mod` playerCount)
                              (addScore marbles newMarble scores playerNum)
                              newMarbles
                              (newMarble + 1)
    | otherwise
    = calculateHighScore' ((playerNum + 1) `mod` playerCount)
                          scores
                          (placeMarble marbles newMarble)
                          (newMarble + 1)

addScore :: Deque Marble -> Marble -> Seq Int -> Int -> Seq Int
addScore marbles newMarble scores playerNum =
  let score             = index scores playerNum
      marbleToBeRemoved = fromMaybe 0 $ D.head (counterClockwise marbles 7)
  in  update playerNum (score + newMarble + marbleToBeRemoved) scores

placeMarble :: Deque Marble -> Marble -> Deque Marble
placeMarble marbles marble = cons marble $ clockwise marbles 2

removeMarble :: Deque Marble -> Deque Marble
removeMarble marbles = case uncons (counterClockwise marbles 7) of
  Just (_, newMarbles) -> newMarbles
  Nothing              -> marbles

counterClockwise :: Deque a -> Int -> Deque a
counterClockwise l distance = iterate D.shiftRight l !! distance

clockwise :: Deque a -> Int -> Deque a
clockwise l distance = iterate D.shiftLeft l !! distance

-------------------------------------------------------
-------- Tests ----------------------------------------
-------------------------------------------------------

tests :: IO ()
tests = do
  testCalculateHighScore
  testPlaceMarble
  testRemoveMarble
  testCounterClockwise
  testClockwise
  testAddScore

testEq :: (Eq a, Show a) => a -> a -> IO ()
testEq a b = putStrLn $ show a ++ " == " ++ show b ++ ": " ++ show (a == b)

testCalculateHighScore :: IO ()
testCalculateHighScore = do
  testEq (calculateHighScore 9 25)    32
  testEq (calculateHighScore 10 1618) 8317
  testEq (calculateHighScore 13 7999) 146373
  testEq (calculateHighScore 17 1104) 2764
  testEq (calculateHighScore 21 6111) 54718
  testEq (calculateHighScore 30 5807) 37305

testPlaceMarble :: IO ()
testPlaceMarble = do
  testEq (toList $ placeMarble (D.fromList [0]) 1)          [1, 0]
  testEq (toList $ placeMarble (D.fromList [1, 0]) 2)       [2, 1, 0]
  testEq (toList $ placeMarble (D.fromList [2, 1, 0]) 3)    [3, 0, 2, 1]
  testEq (toList $ placeMarble (D.fromList [3, 0, 2, 1]) 4) [4, 2, 1, 3, 0]
  testEq (toList $ placeMarble (D.fromList [4, 2, 1, 3, 0]) 5)
         [5, 1, 3, 0, 4, 2]

testRemoveMarble :: IO ()
testRemoveMarble = testEq
  (toList $ removeMarble
    (D.fromList
      [ 22
      , 11
      , 1
      , 12
      , 6
      , 13
      , 3
      , 14
      , 7
      , 15
      , 0
      , 16
      , 8
      , 17
      , 4
      , 18
      , 9
      , 19
      , 2
      , 20
      , 10
      , 21
      , 5
      ]
    )
  )
  [ 19
  , 2
  , 20
  , 10
  , 21
  , 5
  , 22
  , 11
  , 1
  , 12
  , 6
  , 13
  , 3
  , 14
  , 7
  , 15
  , 0
  , 16
  , 8
  , 17
  , 4
  , 18
  ]

testCounterClockwise :: IO ()
testCounterClockwise = do
  testEq (toList $ counterClockwise (D.fromList [1, 2, 3, 4]) 1)
         ([4, 1, 2, 3] :: [Marble])
  testEq (toList $ counterClockwise (D.fromList [1, 2, 3, 4]) 2)
         ([3, 4, 1, 2] :: [Marble])
  testEq (toList $ counterClockwise (D.fromList [1, 2, 3, 4]) 3)
         ([2, 3, 4, 1] :: [Marble])
  testEq (toList $ counterClockwise (D.fromList [1, 2, 3, 4]) 4)
         ([1, 2, 3, 4] :: [Marble])

testClockwise :: IO ()
testClockwise = do
  testEq (toList $ clockwise (D.fromList [1, 2, 3, 4]) 1)
         ([2, 3, 4, 1] :: [Marble])
  testEq (toList $ clockwise (D.fromList [1, 2, 3, 4]) 2)
         ([3, 4, 1, 2] :: [Marble])
  testEq (toList $ clockwise (D.fromList [1, 2, 3, 4]) 3)
         ([4, 1, 2, 3] :: [Marble])
  testEq (toList $ clockwise (D.fromList [1, 2, 3, 4]) 4)
         ([1, 2, 3, 4] :: [Marble])


testAddScore :: IO ()
testAddScore =
  testEq
      (addScore
        (D.fromList
          [ 22
          , 11
          , 1
          , 12
          , 6
          , 13
          , 3
          , 14
          , 7
          , 15
          , 0
          , 16
          , 8
          , 17
          , 4
          , 18
          , 9
          , 19
          , 2
          , 20
          , 10
          , 21
          , 5
          ]
        )
        23
        (S.replicate 9 0)
        4
      )
    $ S.fromList [0, 0, 0, 0, 32, 0, 0, 0, 0]

