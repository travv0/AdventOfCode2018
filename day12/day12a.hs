module Main where

import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntMap
import           Data.List.Split                ( chunksOf
                                                , splitOn
                                                )
import           Data.Maybe                     ( fromMaybe )
import           System.Environment             ( getArgs )

type Pots = IntMap Char
type Notes = [Note]
type Note = (String, Char)

main :: IO ()
main = do
  args <- getArgs
  if "test" `elem` args
    then tests
    else do
      input <- getContents
      let (pots, notes) = parseInput input
      print $ sumPlantNums $ runGenerations notes pots 20

parseNotes :: String -> Notes
parseNotes s =
  map (\[str, _, c] -> (str, head c)) $ chunksOf 3 $ concatMap words $ lines s

parseInput :: String -> (Pots, Notes)
parseInput s =
  let [pots, notes] = splitOn "\n\n" s
  in  (stringToPots 0 (words pots !! 2), parseNotes notes)

runGenerations :: Notes -> Pots -> Int -> Pots
runGenerations notes = runGenerations'
 where
  runGenerations' pots 0 = pots
  runGenerations' pots i = runGenerations'
    (IntMap.fromList $ map
      (\j -> (j, fromMaybe '.' $ getNewPlantState notes (getCompareArea j pots))
      )
      [fst (IntMap.findMin pots) - 2 .. fst (IntMap.findMax pots) + 2]
    )
    (i - 1)

stringToPots :: Int -> String -> Pots
stringToPots i s = IntMap.fromList $ zip [i ..] s

getCompareArea :: Int -> Pots -> String
getCompareArea i pots =
  map (fromMaybe '.' . (`IntMap.lookup` pots)) [i - 2 .. i + 2]

getNewPlantState :: Notes -> String -> Maybe Char
getNewPlantState notes area = lookup area notes

sumPlantNums :: Pots -> Int
sumPlantNums = IntMap.foldrWithKey (\k v r -> if v == '#' then r + k else r) 0

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
testParseNotes = testEq
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
  [ ("...##", '#')
  , ("..#..", '#')
  , (".#...", '#')
  , (".#.#.", '#')
  , (".#.##", '#')
  , (".##..", '#')
  , (".####", '#')
  , ("#.#.#", '#')
  , ("#.###", '#')
  , ("##.#.", '#')
  , ("##.##", '#')
  , ("###..", '#')
  , ("###.#", '#')
  , ("####.", '#')
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
      (runGenerations notes
                      (stringToPots 0 "#..#.#..##......###...###...........")
                      1
      )
    $ stringToPots (-2) "..#...#....#.....#..#..#..#............."
  testEq
      (IntMap.filterWithKey (\k _ -> k >= -3 && k <= 35) $ runGenerations
        notes
        (stringToPots 0 "#..#.#..##......###...###...........")
        20
      )
    $ stringToPots (-3) ".#....##....#####...#######....#.#..##."

testSumPlantNums :: IO ()
testSumPlantNums =
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
  in  testEq
        (sumPlantNums $ runGenerations
          notes
          (stringToPots 0 "#..#.#..##......###...###...........")
          20
        )
        325
