module Lib where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, elemIndex, foldl', partition, sort, (\\))
import Data.Maybe (fromMaybe)

data Worker = Worker Task CurrentSeconds EndSeconds
    deriving (Eq, Show)

type Task = Char

type CurrentSeconds = Seconds
type EndSeconds = Seconds

type Seconds = Int

naiveTopSort :: [(Char, String)] -> String
naiveTopSort graph = naiveTopSort' (sort graph) []
  where
    naiveTopSort' [] sorted = sorted
    naiveTopSort' unsortedGraph sorted =
        let newSorted =
                sorted
                    ++ [ fst $
                            foldl'
                                ( \(srtd, found) (c, deps) ->
                                    if not found && null (deps \\ sorted)
                                        then (c, True)
                                        else (srtd, found)
                                )
                                (fst $ head unsortedGraph, False)
                                unsortedGraph
                       ]
            stillUnsorted = filter (\(c, _) -> c `notElem` newSorted) unsortedGraph
         in naiveTopSort' stillUnsorted newSorted

graphFromString :: String -> [(Char, String)]
graphFromString string =
    let ss = lines $ trim string
        keyDeps =
            foldr
                ( \s l ->
                    let w = words s
                        step = head $ w !! 7
                        stepDependsOn = head $ w !! 1
                        lWithDep = case lookup stepDependsOn l of
                            Just _ -> l
                            Nothing -> (stepDependsOn, []) : l
                     in case lookup step lWithDep of
                            Just deps -> replace lWithDep step (stepDependsOn : deps)
                            Nothing -> (step, [stepDependsOn]) : lWithDep
                )
                []
                ss
     in keyDeps

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

replace :: Eq a => [(a, b)] -> a -> b -> [(a, b)]
replace list key newValue =
    let keyIndices = [y | (y, (k, _)) <- zip [0 :: Int ..] list, k == key]
        (front, _ : back) = case keyIndices of
            (i : _) -> splitAt i list
            [] -> ([], list)
     in front ++ ((key, newValue) : back)

timedParallelTopSort :: [(Char, String)] -> Int -> Seconds -> Int
timedParallelTopSort graph workerCount baseSecondss =
    timedParallelTopSort'
        (sort graph)
        []
        []
        0
  where
    timedParallelTopSort' ::
        [(Char, String)] -> [Worker] -> String -> Seconds -> Seconds
    timedParallelTopSort' [] [] _ secs = secs - 1
    timedParallelTopSort' untouchedGraph workers sorted secs =
        let (finishedWorkers, workingWorkers) = processWorkers workers
            newSorted = sorted ++ map (\(Worker task _ _) -> task) finishedWorkers
            newWorkers =
                workingWorkers
                    ++ foldl'
                        ( \wrkrs (c, deps) ->
                            if null (deps \\ newSorted)
                                && length workingWorkers
                                + length wrkrs
                                < workerCount
                                then wrkrs ++ [newWorker c baseSecondss]
                                else wrkrs
                        )
                        []
                        untouchedGraph
            stillUntouched =
                filter
                    (\(c, _) -> c `notElem` map (\(Worker t _ _) -> t) newWorkers)
                    untouchedGraph
         in timedParallelTopSort' stillUntouched newWorkers newSorted $ secs + 1

newWorker :: Task -> Seconds -> Worker
newWorker task baseSecondss =
    Worker task 0 $ baseSecondss + fromMaybe 0 (elemIndex task ['A' .. 'Z']) + 1

processWorkers :: [Worker] -> ([Worker], [Worker])
processWorkers workers =
    partition (\(Worker _ time endTime) -> time >= endTime) $
        map
            (\(Worker task time endTime) -> Worker task (time + 1) endTime)
            workers
