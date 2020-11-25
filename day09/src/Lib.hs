module Lib where

import Data.Maybe (fromMaybe)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Deque.Lazy (Deque)
import qualified Deque.Lazy as D
import GHC.Exts (fromList)
import Prelude as P

type Marble = Int

parseInput :: String -> (Int, Int)
parseInput s = let w = words s in (read (P.head w), read (w !! 6))

calculateHighScore :: Int -> Marble -> Int
calculateHighScore playerCount lastMarble =
    let scores = S.replicate playerCount 0
     in maximum $ calculateHighScore' 0 scores (fromList [0]) 1
  where
    calculateHighScore' :: Int -> Seq Int -> Deque Marble -> Marble -> Seq Int
    calculateHighScore' playerNum scores marbles newMarble
        | newMarble > lastMarble =
            scores
        | newMarble `mod` 23 == 0 =
            let newMarbles = removeMarble marbles
             in calculateHighScore'
                    ((playerNum + 1) `mod` playerCount)
                    (addScore marbles newMarble scores playerNum)
                    newMarbles
                    (newMarble + 1)
        | otherwise =
            calculateHighScore'
                ((playerNum + 1) `mod` playerCount)
                scores
                (placeMarble marbles newMarble)
                (newMarble + 1)

addScore :: Deque Marble -> Marble -> Seq Int -> Int -> Seq Int
addScore marbles newMarble scores playerNum =
    let score = S.index scores playerNum
        marbleToBeRemoved = fromMaybe 0 $ D.head (counterClockwise marbles 7)
     in S.update playerNum (score + newMarble + marbleToBeRemoved) scores

placeMarble :: Deque Marble -> Marble -> Deque Marble
placeMarble marbles marble = D.cons marble $ clockwise marbles 2

removeMarble :: Deque Marble -> Deque Marble
removeMarble marbles = case D.uncons (counterClockwise marbles 7) of
    Just (_, newMarbles) -> newMarbles
    Nothing -> marbles

counterClockwise :: Deque a -> Int -> Deque a
counterClockwise l distance = iterate D.shiftRight l !! distance

clockwise :: Deque a -> Int -> Deque a
clockwise l distance = iterate D.shiftLeft l !! distance
