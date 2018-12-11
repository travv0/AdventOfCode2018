module Main where

import           Data.List
import           Data.Char
import           Data.Maybe

data Worker = Worker Task CurrentSeconds EndSeconds
  deriving (Eq, Show)

type Task = Char

type CurrentSeconds = Seconds
type EndSeconds = Seconds

type Seconds = Int

main :: IO ()
main = do
  graph <- graphFromString <$> getContents
  print $ timedParallelTopSort graph 5 60

graphFromString :: String -> [(Char, String)]
graphFromString string =
  let ss      = lines $ trim string
      keyDeps = foldr
        (\s l ->
          let w             = words s
              step          = head $ w !! 7
              stepDependsOn = head $ w !! 1
              lWithDep      = case lookup stepDependsOn l of
                Just _  -> l
                Nothing -> (stepDependsOn, []) : l
          in  case lookup step lWithDep of
                Just deps -> replace lWithDep step (stepDependsOn : deps)
                Nothing   -> (step, [stepDependsOn]) : lWithDep
        )
        []
        ss
  in  keyDeps

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

replace :: Eq a => [(a, b)] -> a -> b -> [(a, b)]
replace list key newValue =
  let keyIndices = [ y | (y, (k, _)) <- zip [0 :: Int ..] list, k == key ]
      (front, _ : back) = case keyIndices of
        (i : _) -> splitAt i list
        []      -> ([], list)
  in  front ++ ((key, newValue) : back)

timedParallelTopSort :: [(Char, String)] -> Int -> Seconds -> Int
timedParallelTopSort graph workerCount baseSecondss = timedParallelTopSort'
  (sort graph)
  []
  []
  0
 where
  timedParallelTopSort'
    :: [(Char, String)] -> [Worker] -> String -> Seconds -> Seconds
  timedParallelTopSort' [] [] _ secs = secs - 1
  timedParallelTopSort' untouchedGraph workers sorted secs =
    let (finishedWorkers, workingWorkers) = processWorkers workers
        newSorted = sorted ++ map (\(Worker task _ _) -> task) finishedWorkers
        newWorkers =
          workingWorkers
            ++ foldl'
                 (\wrkrs (c, deps) ->
                   if null (deps \\ newSorted)
                        && length workingWorkers
                        +  length wrkrs
                        <  workerCount
                     then wrkrs ++ [newWorker c baseSecondss]
                     else wrkrs
                 )
                 []
                 untouchedGraph
        stillUntouched = filter
          (\(c, _) -> c `notElem` map (\(Worker t _ _) -> t) newWorkers)
          untouchedGraph
    in  timedParallelTopSort' stillUntouched newWorkers newSorted $ secs + 1

newWorker :: Task -> Seconds -> Worker
newWorker task baseSecondss =
  Worker task 0 $ baseSecondss + fromMaybe 0 (elemIndex task ['A' .. 'Z']) + 1

processWorkers :: [Worker] -> ([Worker], [Worker])
processWorkers workers =
  partition (\(Worker _ time endTime) -> time >= endTime) $ map
    (\(Worker task time endTime) -> Worker task (time + 1) endTime)
    workers

-------------------------------------------------------
-------- Tests ----------------------------------------
-------------------------------------------------------

tests :: IO ()
tests = do
  testNewWorker
  testProcessWorkers
  testTimedParallelTopSort

testEq :: (Eq a, Show a) => a -> a -> IO ()
testEq a b = putStrLn $ show a ++ " == " ++ show b ++ ": " ++ show (a == b)

testNewWorker :: IO ()
testNewWorker = do
  testEq (newWorker 'C' 60) $ Worker 'C' 0 63
  testEq (newWorker 'Z' 60) $ Worker 'Z' 0 86
  testEq (newWorker 'A' 0) $ Worker 'A' 0 1

testProcessWorkers :: IO ()
testProcessWorkers = testEq
  (processWorkers [Worker 'A' 3 5, Worker 'B' 4 5, Worker 'C' 1 8])
  ([Worker 'B' 5 5], [Worker 'A' 4 5, Worker 'C' 2 8])

testTimedParallelTopSort :: IO ()
testTimedParallelTopSort = testEq
  (timedParallelTopSort
    [('F', "C"), ('A', "C"), ('B', "A"), ('C', ""), ('E', "BDF"), ('D', "A")]
    2
    0
  )
  15
