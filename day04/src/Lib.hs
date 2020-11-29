module Lib where

import Control.Lens (at, non, (%~), (&))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.List (isInfixOf, sort)
import Text.Regex.TDFA ((=~))

countSleep :: [String] -> IntMap (IntMap Int)
countSleep entries =
    let entries' = sort entries
     in countSleep' entries' Nothing Nothing IntMap.empty
  where
    countSleep' :: [String] -> Maybe Int -> Maybe Int -> IntMap (IntMap Int) -> IntMap (IntMap Int)
    countSleep' [] _ _ counts = counts
    countSleep' (entry : rest) guardId sleepStartTime counts
        | '#' `elem` entry =
            let (_, _, _, [i]) =
                    entry =~ "#([0-9]+)" :: (String, String, String, [String])
             in countSleep' rest (Just (read i)) sleepStartTime counts
        | "falls asleep" `isInfixOf` entry =
            let (_, _, _, [time]) =
                    entry =~ ":([0-9][0-9])\\]" :: (String, String, String, [String])
             in countSleep' rest guardId (Just (read time)) counts
    countSleep' (entry : rest) (Just guardId) (Just sleepStartTime) counts =
        let (_, _, _, [time]) =
                entry =~ ":([0-9][0-9])\\]" :: (String, String, String, [String])
         in countSleep'
                rest
                (Just guardId)
                Nothing
                $ foldr
                    ( \time' counts' ->
                        counts'
                            & at guardId
                                . non IntMap.empty
                                . at time'
                                . non 0
                            %~ succ
                    )
                    counts
                    [sleepStartTime .. read time - 1]
    countSleep' (entry : _) guardId sleepStartTime _ =
        error $
            "Invalid state: guardId = "
                <> show guardId
                <> ", sleepStartTime = "
                <> show sleepStartTime
                <> ", entry =\""
                <> entry
                <> "\""

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

mostMinutesTime :: IntMap Int -> (Int, Int)
mostMinutesTime =
    IntMap.foldrWithKey
        ( \time sleep acc@(_, maxSleep) ->
            if maxSleep < sleep
                then (time, sleep)
                else acc
        )
        (0, 0)

mostSameMinuteId :: IntMap (IntMap Int) -> (Int, Int)
mostSameMinuteId counts =
    let (guardId, time, _) =
            IntMap.foldrWithKey
                ( \guardId' time' acc@(_, _, maxSleep) ->
                    let (maxTime, sleep) = mostMinutesTime time'
                     in if maxSleep < sleep
                            then (guardId', maxTime, sleep)
                            else acc
                )
                (0, 0, 0)
                counts
     in (guardId, time)
