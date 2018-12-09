{-# LANGUAGE MultiWayIf #-}

module Main where

import           Data.List
import           Data.Char
import           Data.Tuple

type Coord = (Int, Int)

data Bounds =
  Bounds { xMin :: Int
         , xMax :: Int
         , yMin :: Int
         , yMax :: Int
         } deriving Show

maximumDistance :: Int
maximumDistance = 10000

main :: IO ()
main = interact (show . length . notToFar maximumDistance . parseCoords . lines)

parseCoords :: [String] -> [Coord]
parseCoords = map
  (\s ->
    let [x, y] = wordsWhen (== ',') (trim s) in (read x :: Int, read y :: Int)
  )

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
  "" -> []
  s' -> w : wordsWhen p s'' where (w, s'') = break p s'

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

distance :: Coord -> Coord -> Int
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

bounds :: [Coord] -> Bounds
bounds coords = Bounds lowX highX lowY highY
 where
  lowX  = foldr (min . fst) (maxBound :: Int) coords
  highX = foldr (max . fst) 0 coords
  lowY  = foldr (min . snd) (maxBound :: Int) coords
  highY = foldr (max . snd) 0 coords

allSurroundingCoords :: [Coord] -> [Coord]
allSurroundingCoords coords = concat
  $ map (\y -> map (swap . (,) y) [0 .. xMax b]) [0 .. yMax b]
  where b = bounds coords

totalDistance :: [Coord] -> Coord -> Int
totalDistance coords coord = sum $ map (distance coord) coords

notToFar :: Int -> [Coord] -> [Coord]
notToFar maxDistance coords = filter ((<maxDistance) . totalDistance coords) $ allSurroundingCoords coords
