import Lib

import Control.Monad (unless)
import System.Exit (exitFailure)

main :: IO ()
main = do
    testNaiveTopSort
    testNewWorker
    testProcessWorkers
    testTimedParallelTopSort

testEq :: (Eq a, Show a) => a -> a -> IO ()
testEq a b = do
    let check = a == b
    putStrLn $ show a ++ " == " ++ show b ++ ": " ++ show check
    unless check exitFailure

testNaiveTopSort :: IO ()
testNaiveTopSort =
    testEq
        ( naiveTopSort
            [('F', "C"), ('A', "C"), ('B', "A"), ('C', ""), ('E', "BDF"), ('D', "A")]
        )
        "CABDFE"

testNewWorker :: IO ()
testNewWorker = do
    testEq (newWorker 'C' 60) $ Worker 'C' 0 63
    testEq (newWorker 'Z' 60) $ Worker 'Z' 0 86
    testEq (newWorker 'A' 0) $ Worker 'A' 0 1

testProcessWorkers :: IO ()
testProcessWorkers =
    testEq
        (processWorkers [Worker 'A' 3 5, Worker 'B' 4 5, Worker 'C' 1 8])
        ([Worker 'B' 5 5], [Worker 'A' 4 5, Worker 'C' 2 8])

testTimedParallelTopSort :: IO ()
testTimedParallelTopSort =
    testEq
        ( timedParallelTopSort
            [('F', "C"), ('A', "C"), ('B', "A"), ('C', ""), ('E', "BDF"), ('D', "A")]
            2
            0
        )
        15
