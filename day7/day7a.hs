module Main where

import           Data.List
import           Data.Char

main :: IO ()
main = do
  graph <- graphFromString <$> getContents
  print $ naiveTopSort $ sort graph

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

naiveTopSort :: [(Char, String)] -> String
naiveTopSort graph = naiveTopSort' (sort graph) []
 where
  naiveTopSort' [] sorted = sorted
  naiveTopSort' unsortedGraph sorted
    = let
        newSorted =
          sorted
            ++ [ fst $ foldl'
                   (\(srtd, found) (c, deps) ->
                     if not found && null (deps \\ sorted)
                       then (c, True)
                       else (srtd, found)
                   )
                   (fst $ head unsortedGraph, False)
                   unsortedGraph
               ]
        stillUnsorted = filter (\(c, _) -> c `notElem` newSorted) unsortedGraph
      in
        naiveTopSort' stillUnsorted newSorted

-------------------------------------------------------
-------- Tests ----------------------------------------
-------------------------------------------------------

tests :: IO ()
tests = testNaiveTopSort

testEq :: (Eq a, Show a) => a -> a -> IO ()
testEq a b = putStrLn $ show a ++ " == " ++ show b ++ ": " ++ show (a == b)

testNaiveTopSort :: IO ()
testNaiveTopSort = testEq
  (naiveTopSort $ sort
    [('F', "C"), ('A', "C"), ('B', "A"), ('C', ""), ('E', "BDF"), ('D', "A")]
  )
  "CABDFE"
