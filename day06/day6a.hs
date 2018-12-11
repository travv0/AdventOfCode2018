module Main where

import           Data.List
import           Data.Char
import           Data.Maybe
import           Data.Tuple
import           Control.Monad

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
boundaryPoints b = filter (edgePoint b)
  $ concatMap (\y -> map (swap . (,) y) [xMin b .. xMax b]) [yMin b .. yMax b]

closestCoord :: [Coord] -> Coord -> Maybe Coord
closestCoord coords fromCoord =
  case
      sortBy (\c d -> distance fromCoord c `compare` distance fromCoord d)
             coords
    of
      (c : d : _) ->
        if distance fromCoord c == distance fromCoord d then Nothing else Just c
      (c : _) -> Just c
      _       -> Nothing

findMaxClosest :: [Coord] -> Int
findMaxClosest coords = maximum $ map
  (\coord -> length $ filter (\c -> c == Just coord) closestCoords)
  finitePoints
 where
  closestCoords = map (closestCoord coords) $ allSurroundingCoords coords
  finitePoints =
    filter (not . flip elem (infinitePoints (bounds coords) coords)) coords

allSurroundingCoords :: [Coord] -> [Coord]
allSurroundingCoords coords = concatMap
  (\y -> map (swap . (,) y) [0 .. xMax b])
  [0 .. yMax b]
  where b = bounds coords

printGrid :: [Coord] -> Int -> Int -> IO ()
printGrid coords width height = do
  let letteredCoords = zip coords ['A' ..]
      closestCoords  = map (closestCoord coords) $ gridCoords width height
  mapM_
      (\(i, mCoord) -> do
        let x = i `mod` width
            y = i `div` width
        case mCoord of
          Just coord -> if (x, y) `elem` coords
            then putStr [fromMaybe '.' $ lookup coord letteredCoords]
            else putStr [toLower $ fromMaybe '.' $ lookup coord letteredCoords]
          Nothing -> putStr "."
        when (i `mod` width == width - 1) $ putStr "\n"
      )
    $ zip [0 ..] closestCoords

gridCoords :: Int -> Int -> [Coord]
gridCoords width height =
  concatMap (\y -> map (swap . (,) y) [0 .. width - 1]) [0 .. height - 1]
