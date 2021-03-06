module Lib where

import Data.Char (isLower, isSpace, isUpper, toLower, toUpper)
import Data.List (dropWhileEnd, foldl')

findShortestReduction :: String -> Int
findShortestReduction s =
    snd $ foldr (rememberShortest s) ('a', maxBound :: Int) ['a' .. 'z']

rememberShortest :: String -> Char -> (Char, Int) -> (Char, Int)
rememberShortest s c (shortestChar, shortestLen) =
    let reducedPolymerLength = length $ reduce (filter (\x -> toLower x /= c) s)
     in if reducedPolymerLength < shortestLen
            then (c, reducedPolymerLength)
            else (shortestChar, shortestLen)

reduce :: String -> String
reduce = reverse . foldl' addOrReduce "" . trim

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

addOrReduce :: String -> Char -> String
addOrReduce s c =
    if not (null s) && head s == invertCase c then tail s else c : s

invertCase :: Char -> Char
invertCase c
    | isUpper c = toLower c
    | isLower c = toUpper c
    | otherwise = c