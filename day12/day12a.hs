module Main where

import           Data.IntMap.Strict             ( IntMap )
import qualified Data.IntMap.Strict            as IntMap
import           Data.List.Split                ( chunksOf )
import           Data.Maybe                     ( fromMaybe )
import           System.Environment             ( getArgs )

type Pots = IntMap Char
type Notes = [Note]
type Note = (String, Char)

main :: IO ()
main = do
  args <- getArgs
  if "test" `elem` args then tests else undefined

parseNotes :: String -> Notes
parseNotes s =
  map (\[str, _, c] -> (str, head c)) $ chunksOf 3 $ concatMap words $ lines s

runGenerations :: String -> Notes -> Int -> Pots
runGenerations s notes count =
  let pots = stringToPots 0 s in runGenerations' pots count
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

-------------------------------------------------------
-------- Tests ----------------------------------------
-------------------------------------------------------

tests :: IO ()
tests = do
  testParseNotes
  testRunGenerations

testEq :: (Eq a, Show a) => a -> a -> IO ()
testEq a b = putStrLn $ show a ++ " == " ++ show b ++ ": " ++ show (a == b)

testParseNotes :: IO ()
testParseNotes = testEq
  (parseNotes
    "...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #"
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
  let
    notes =
      parseNotes
        "...## => #\n..#.. => #\n.#... => #\n.#.#. => #\n.#.## => #\n.##.. => #\n.#### => #\n#.#.# => #\n#.### => #\n##.#. => #\n##.## => #\n###.. => #\n###.# => #\n####. => #"
  testEq (runGenerations "#..#.#..##......###...###..........." notes 1)
    $ stringToPots 0 "#...#....#.....#..#..#..#..........."
