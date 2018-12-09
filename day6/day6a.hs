{-# LANGUAGE MultiWayIf #-}

module Main where

import           Data.List
import           Data.Char
import           Data.Maybe

type Coord = (Int, Int)

data Bounds =
  Bounds { xMin :: Int
         , xMax :: Int
         , yMin :: Int
         , yMax :: Int
         } deriving Show

main :: IO ()
main = interact (show . findMaxClosest . parseCoords . lines)

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

edgePoint :: Bounds -> Coord -> Bool
edgePoint b (x, y) = x == xMin b || x == xMax b || y == yMin b || y == yMax b

infinitePoints :: Bounds -> [Coord] -> [Coord]
infinitePoints b coords =
  nub
    $ map fromJust
    $ filter isJust
    $ map (closestCoord coords)
    $ boundaryPoints b

boundaryPoints :: Bounds -> [Coord]
boundaryPoints b = filter (edgePoint b) $ concat $ map
  (\x -> map ((,) x) [yMin b .. yMax b])
  [xMin b .. xMax b]

closestCoord :: [Coord] -> Coord -> Maybe Coord
closestCoord coords coord =
  foldr (getCloserCoord coord) (Just $ head coords) (tail coords)

getCloserCoord :: Coord -> Coord -> Maybe Coord -> Maybe Coord
getCloserCoord fromCoord newCoord mCloseCoord = case mCloseCoord of
  Just closeCoord ->
    let newDist = distance newCoord fromCoord
        minDist = distance closeCoord fromCoord
    in  if
          | newDist < minDist  -> Just newCoord
          | newDist == minDist -> Nothing
          | otherwise          -> mCloseCoord
  Nothing -> Nothing

findMaxClosest :: [Coord] -> Int
findMaxClosest coords = maximum $ map
  (\coord ->
    length $ filter (\c -> isJust c && fromJust c == coord) closestCoords
  )
  finitePoints
 where
  closestCoords = map (closestCoord coords) $ allSurroundingCoords coords
  finitePoints =
    filter (not . (flip elem) (infinitePoints (bounds coords) coords)) coords

allSurroundingCoords :: [Coord] -> [Coord]
allSurroundingCoords coords = concat
  $ map (\x -> map ((,) x) [yMin b .. yMax b]) [xMin b .. xMax b]
  where b = bounds coords
