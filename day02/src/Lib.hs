module Lib where

import Data.List (nub)

generateChecksum :: [String] -> Int
generateChecksum ids =
    let tats = map hasTwosAndThrees ids
        (twos, threes) =
            foldr
                ( \(b2, b3) (tws, ths) ->
                    (if b2 then tws + 1 else tws, if b3 then ths + 1 else ths)
                )
                (0, 0)
                tats
     in twos * threes

hasTwosAndThrees :: String -> (Bool, Bool)
hasTwosAndThrees s =
    let counts = map (\c -> length $ filter (== c) s) $ nub s
     in (2 `elem` counts, 3 `elem` counts)

findMatch :: [String] -> String
findMatch ids =
    let idLen = length (head ids)
     in head $
            filter
                (\s -> length s == idLen - 1)
                (concatMap (getMatchingChars ids) ids)

getMatchingChars :: [String] -> String -> [String]
getMatchingChars ids i =
    map (foldr (\(c1, c2) s -> if c1 == c2 then c1 : s else s) [] . zip i) ids