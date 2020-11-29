module Lib where

import Control.Lens (At (at), non, (%~), (&))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (isInfixOf, sort)
import Data.Maybe (fromJust)
import Text.Regex.TDFA ((=~))

countSleep :: [String] -> IntMap (IntMap Int)
countSleep entries =
    let entries' = sort entries
     in countSleep' entries' Nothing Nothing IntMap.empty
  where
    countSleep' :: [String] -> Maybe Int -> Maybe Int -> IntMap (IntMap Int) -> IntMap (IntMap Int)
    countSleep' entries' guardId sleepStartTime counts =
        case entries' of
            [] -> counts
            entry : rest
                | '#' `elem` entry ->
                    let (_, _, _, [i]) =
                            entry =~ "#([0-9]+)" :: (String, String, String, [String])
                     in countSleep' rest (Just (read i)) sleepStartTime counts
                | "falls asleep" `isInfixOf` entry ->
                    let (_, _, _, [time]) =
                            entry =~ ":([0-9][0-9])\\]" :: (String, String, String, [String])
                     in countSleep' rest guardId (Just (read time)) counts
                | otherwise ->
                    let (_, _, _, [time]) =
                            entry =~ ":([0-9][0-9])\\]" :: (String, String, String, [String])
                     in countSleep'
                            rest
                            guardId
                            Nothing
                            $ foldr
                                ( \time' counts' ->
                                    counts' & at (fromJust guardId) . non IntMap.empty . at time' . non 0 %~ (+ 1)
                                )
                                counts
                                [fromJust sleepStartTime .. read time - 1]

mostMinutesId :: IntMap (IntMap Int) -> Int
mostMinutesId =
    fst
        . IntMap.foldrWithKey
            ( \guardId time acc@(_, maxSleep) ->
                let sleep = IntMap.foldr (+) 0 time
                 in if maxSleep < sleep
                        then (guardId, sleep)
                        else acc
            )
            (0, 0)

mostMinutesTime :: IntMap Int -> Int
mostMinutesTime =
    fst
        . IntMap.foldrWithKey
            ( \time sleep acc@(_, maxSleep) ->
                if maxSleep < sleep
                    then (time, sleep)
                    else acc
            )
            (0, 0)