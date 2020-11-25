module Lib where

import Control.Applicative (liftA2)
import Control.Parallel.Strategies (parMap, rdeepseq)
import Data.Vector.Unboxed (Vector, fromList, (!))

type Grid = Vector Int

type Cell = (Coord, Coord)

type Coord = Int
type SerialNum = Int

format2dCoords :: (Coord, Coord) -> String
format2dCoords (x, y) = show x <> "," <> show y

format3dCoords :: (Coord, Coord, Coord) -> String
format3dCoords (x, y, z) = show x <> "," <> show y <> "," <> show z

findVarSizePowerfulSquare :: Grid -> Int -> (Coord, Coord, Int)
findVarSizePowerfulSquare grid gridSize =
    fst $
        foldr
            ( \(size, ((x, y), power)) currMax@(_, maxPower) ->
                if power > maxPower then ((x, y, size), power) else currMax
            )
            ((0, 0, 0), 0)
            $ parMap
                rdeepseq
                (\size -> (size, findPowerfulSquare grid gridSize size))
                [1 .. gridSize]

findPowerfulSquare :: Grid -> Int -> Int -> (Cell, Int)
findPowerfulSquare grid gridSize squareSize =
    foldr
        ( \cell@(x, y) currMax@(_, maxPower) ->
            let cellPower = sumSquare grid gridSize squareSize x y
             in if cellPower > maxPower then (cell, cellPower) else currMax
        )
        ((0, 0), 0)
        $ combine [1 .. gridSize - squareSize + 1] [1 .. gridSize - squareSize + 1]

powerLevel :: Coord -> Coord -> SerialNum -> Int
powerLevel x y serialNum =
    let rackId = x + 10
     in hundredsPlace (((rackId * y) + serialNum) * rackId) - 5

hundredsPlace :: (Integral a) => a -> a
hundredsPlace x =
    let rightTrimmed = x `quot` 100 in snd $ rightTrimmed `quotRem` 10

generateGrid :: Int -> SerialNum -> Grid
generateGrid size serialNum =
    fromList $
        map
            (\i -> powerLevel ((i `rem` size) + 1) ((i `quot` size) + 1) serialNum)
            [0 .. size * size - 1]

getCellValue :: Grid -> Int -> Coord -> Coord -> Int
getCellValue grid size x y = grid ! ((x - 1) + (y - 1) * size)

sumSquare :: Grid -> Int -> Int -> Coord -> Coord -> Int
sumSquare grid gridSize squareSize x y =
    sum $
        concatMap
            (\i -> map (getCellValue grid gridSize i) [y .. y + (squareSize - 1)])
            [x .. x + (squareSize - 1)]

combine :: [a] -> [b] -> [(a, b)]
combine = liftA2 (,)