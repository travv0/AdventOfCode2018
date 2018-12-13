module Main where

import           Data.Bits
import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntMap
import           Data.Char
import           Data.List.Split                ( chunksOf
                                                , splitOn
                                                )
import           Numeric                        ( showIntAtBase )
import           System.Environment             ( getArgs )

type Pots = Integer
type Pot = Integer
type Notes = IntMap Pot

main :: IO ()
main = do
  args <- getArgs
  if "test" `elem` args
    then tests
    else do
      input <- getContents
      let (pots  , notes  ) = parseInput input
          (offset, endPots) = runGenerations notes pots 50000000000
      print $ sumPlantNums offset endPots

parseNotes :: String -> Notes
parseNotes s =
  IntMap.fromList
    $ map (\[str, _, c] -> (fromIntegral (stringToPots str), stringToPots c))
    $ chunksOf 3
    $ concatMap words
    $ lines s

parseInput :: String -> (Pots, Notes)
parseInput s =
  let [pots, notes] = splitOn "\n\n" s
  in  (stringToPots (words pots !! 2), parseNotes notes)

runGenerations :: Notes -> Pots -> Integer -> (Integer, Pots)
runGenerations notes = runGenerations' 0
 where
  runGenerations' :: Integer -> Pots -> Integer -> (Integer, Pots)
  runGenerations' offset pots 0 = (offset, pots)
  runGenerations' offset pots count =
    let
      shft p = countTrailingZeros (fromIntegral p :: Int) - 2
      shiftedPots = shiftR pots $ shft pots
      updatedPots = foldr
        (\i p -> case getNewPlantState notes (getCompareArea i shiftedPots) of
          Just 1 -> setBit p i
          _      -> clearBit p i
        )
        shiftedPots
        [0 .. potCount shiftedPots + 2]
    in
      if shiftedPots == shiftR updatedPots (shft updatedPots)
        then (offset + count * toInteger (shft updatedPots), pots)
        else runGenerations' (offset + toInteger (shft pots))
                             updatedPots
                             (count - 1)

potCount :: Pots -> Int
potCount = (+ 1) . length . takeWhile (> 1) . iterate (`shiftR` 1)

stringToPots :: String -> Pots
stringToPots =
  foldr (\(i, c) b -> if c == '#' then setBit b i else b) 0 . zip [0 ..]

showBits :: Integer -> String
showBits x = showIntAtBase 2 intToDigit x ""

showPots :: Pots -> String
showPots x = reverse $ showIntAtBase 2 (\i -> if i == 1 then '#' else '.') x ""

getCompareArea :: Int -> Pots -> Pots
getCompareArea i pots = maskBits 5 $ shiftR pots (i - 2)

maskBits :: Int -> Integer -> Integer
maskBits n bits = bits .&. (shiftL 1 n - 1)

getNewPlantState :: Notes -> Pots -> Maybe Pot
getNewPlantState notes area = IntMap.lookup (fromIntegral area) notes

sumPlantNums :: Integer -> Pots -> Integer
sumPlantNums i =
  foldr (\(k, v) r -> if v == '#' then r + k else r) 0 . zip [i ..] . showPots

-------------------------------------------------------
-------- Tests ----------------------------------------
-------------------------------------------------------

tests :: IO ()
tests = do
  testParseNotes
  testRunGenerations
  testSumPlantNums

testEq :: (Eq a, Show a) => a -> a -> IO ()
testEq a b = putStrLn $ show a ++ " == " ++ show b ++ ": " ++ show (a == b)

testParseNotes :: IO ()
testParseNotes =
  testEq
      (  parseNotes
      $  "...## => #\n"
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
        parseNotes
          $  "...## => #\n"
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
      (snd $ runGenerations
        notes
        (stringToPots "#..#.#..##......###...###...........")
        1
      )
    $ stringToPots "..#...#....#.....#..#..#..#............."
  testEq
      (snd $ runGenerations
        notes
        (stringToPots "#..#.#..##......###...###...........")
        20
      )
    $ stringToPots "..#....##....#####...#######....#.#..##."

testSumPlantNums :: IO ()
testSumPlantNums = do
  let notes =
        parseNotes
          $  "...## => #\n"
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
      (offset, pots) = runGenerations
        notes
        (stringToPots "#..#.#..##......###...###...........")
        20

  testEq (sumPlantNums offset pots) 325
