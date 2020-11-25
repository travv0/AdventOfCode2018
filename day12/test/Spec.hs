import Lib

import Control.Monad (unless)
import qualified Data.IntMap.Strict as IntMap
import System.Exit (exitFailure)

main :: IO ()
main = do
    testParseNotes
    testRunGenerations
    testSumPlantNums

testEq :: (Eq a, Show a) => a -> a -> IO ()
testEq a b = do
    let check = a == b
    putStrLn $ show a ++ " == " ++ show b ++ ": " ++ show check
    unless check exitFailure

testParseNotes :: IO ()
testParseNotes =
    testEq
        ( parseNotes $
            "...## => #\n"
                ++ "..#.. => #\n"
                ++ ".#... => #\n"
                ++ ".#.#. => #\n"
                ++ ".#.## => #\n"
                ++ ".##.. => #\n"
                ++ ".#### => #\n"
                ++ "#.#.# => #\n"
                ++ "#.### => #\n"
                ++ "##.#. => #\n"
                ++ "##.## => #\n"
                ++ "###.. => #\n"
                ++ "###.# => #\n"
                ++ "####. => #"
        )
        $ IntMap.fromList
            [ (fromIntegral $ stringToPots "...##", stringToPots "#")
            , (fromIntegral $ stringToPots "..#..", stringToPots "#")
            , (fromIntegral $ stringToPots ".#...", stringToPots "#")
            , (fromIntegral $ stringToPots ".#.#.", stringToPots "#")
            , (fromIntegral $ stringToPots ".#.##", stringToPots "#")
            , (fromIntegral $ stringToPots ".##..", stringToPots "#")
            , (fromIntegral $ stringToPots ".####", stringToPots "#")
            , (fromIntegral $ stringToPots "#.#.#", stringToPots "#")
            , (fromIntegral $ stringToPots "#.###", stringToPots "#")
            , (fromIntegral $ stringToPots "##.#.", stringToPots "#")
            , (fromIntegral $ stringToPots "##.##", stringToPots "#")
            , (fromIntegral $ stringToPots "###..", stringToPots "#")
            , (fromIntegral $ stringToPots "###.#", stringToPots "#")
            , (fromIntegral $ stringToPots "####.", stringToPots "#")
            ]

testRunGenerations :: IO ()
testRunGenerations = do
    let notes =
            parseNotes $
                "...## => #\n"
                    ++ "..#.. => #\n"
                    ++ ".#... => #\n"
                    ++ ".#.#. => #\n"
                    ++ ".#.## => #\n"
                    ++ ".##.. => #\n"
                    ++ ".#### => #\n"
                    ++ "#.#.# => #\n"
                    ++ "#.### => #\n"
                    ++ "##.#. => #\n"
                    ++ "##.## => #\n"
                    ++ "###.. => #\n"
                    ++ "###.# => #\n"
                    ++ "####. => #"
    testEq
        ( snd $
            runGenerations
                notes
                (stringToPots "#..#.#..##......###...###...........")
                1
        )
        $ stringToPots "..#...#....#.....#..#..#..#............."
    testEq
        ( snd $
            runGenerations
                notes
                (stringToPots "#..#.#..##......###...###...........")
                20
        )
        $ stringToPots "..#....##....#####...#######....#.#..##."

testSumPlantNums :: IO ()
testSumPlantNums = do
    let notes =
            parseNotes $
                "...## => #\n"
                    ++ "..#.. => #\n"
                    ++ ".#... => #\n"
                    ++ ".#.#. => #\n"
                    ++ ".#.## => #\n"
                    ++ ".##.. => #\n"
                    ++ ".#### => #\n"
                    ++ "#.#.# => #\n"
                    ++ "#.### => #\n"
                    ++ "##.#. => #\n"
                    ++ "##.## => #\n"
                    ++ "###.. => #\n"
                    ++ "###.# => #\n"
                    ++ "####. => #"
        (offset, pots) =
            runGenerations
                notes
                (stringToPots "#..#.#..##......###...###...........")
                20

    testEq (sumPlantNums offset pots) 325