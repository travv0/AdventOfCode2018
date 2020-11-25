module Lib where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, nub, sortBy)
import Data.Maybe (mapMaybe)
import Data.Tuple (swap)

type Coord = (Int, Int)

data Bounds = Bounds
    { xMin :: Int
    , xMax :: Int
    , yMin :: Int
    , yMax :: Int
    }
    deriving (Show)

maximumDistance :: Int
maximumDistance = 10000

findMaxClosest :: [Coord] -> Int
findMaxClosest coords =
    maximum $
        map
            (\coord -> length $ filter (\c -> c == Just coord) closestCoords)
            finitePoints
  where
    closestCoords = map (closestCoord coords) $ allSurroundingCoords coords
    finitePoints =
        filter (not . flip elem (infinitePoints (bounds coords) coords)) coords

closestCoord :: [Coord] -> Coord -> Maybe Coord
closestCoord coords fromCoord =
    case sortBy
        (\c d -> distance fromCoord c `compare` distance fromCoord d)
        coords of
        (c : d : _) ->
            if distance fromCoord c == distance fromCoord d then Nothing else Just c
        (c : _) -> Just c
        _ -> Nothing

infinitePoints :: Bounds -> [Coord] -> [Coord]
infinitePoints b coords =
    nub $
        mapMaybe (closestCoord coords) $
            boundaryPoints b

boundaryPoints :: Bounds -> [Coord]
boundaryPoints b =
    filter (edgePoint b) $
        concatMap (\y -> map (swap . (,) y) [xMin b .. xMax b]) [yMin b .. yMax b]

edgePoint :: Bounds -> Coord -> Bool
edgePoint b (x, y) = x == xMin b || x == xMax b || y == yMin b || y == yMax b

parseCoords :: [String] -> [Coord]
parseCoords =
    map
        ( \s ->
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
    lowX = foldr (min . fst) (maxBound :: Int) coords
    highX = foldr (max . fst) 0 coords
    lowY = foldr (min . snd) (maxBound :: Int) coords
    highY = foldr (max . snd) 0 coords

allSurroundingCoords :: [Coord] -> [Coord]
allSurroundingCoords coords =
    concatMap
        (\y -> map (swap . (,) y) [0 .. xMax b])
        [0 .. yMax b]
  where
    b = bounds coords

totalDistance :: [Coord] -> Coord -> Int
totalDistance coords coord = sum $ map (distance coord) coords

notTooFar :: Int -> [Coord] -> [Coord]
notTooFar maxDistance coords =
    filter ((< maxDistance) . totalDistance coords) $ allSurroundingCoords coords