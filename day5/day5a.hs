module Main where

import           Data.List
import           Data.Char

main :: IO ()
main = interact (show . length . reduce)

reduce :: String -> String
reduce = reverse . foldl' addOrReduce "" . trim

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

addOrReduce :: String -> Char -> String
addOrReduce s c =
  if not (null s) && head s == invertCase c then tail s else c : s

invertCase :: Char -> Char
invertCase c | isUpper c = toLower c
             | isLower c = toUpper c
             | otherwise = c
