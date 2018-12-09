module Main where

import           Data.Graph
import           Data.List
import           Data.Char
import           Debug.Trace

main :: IO ()
main = do
  (graph, vertexToNode, _) <- graphFromString <$> getContents
  print $ reverse $ map (\v -> let (c, _, _) = vertexToNode v in c) $ topSort
    graph

graphFromString
  :: String -> (Graph, Vertex -> (Char, Char, [Char]), Char -> Maybe Vertex)
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
  in  trace (show $ map (\(k, v) -> (k, k, v)) keyDeps) $ graphFromEdges $ map
        (\(k, v) -> (k, k, v))
        keyDeps

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

replace :: Eq a => [(a, b)] -> a -> b -> [(a, b)]
replace list key newValue =
  let keyIndices = [ y | (y, (k, v)) <- zip [0 :: Int ..] list, k == key ]
      (front, (_ : back)) = case keyIndices of
        (i : _) -> splitAt i list
        []      -> ([], list)
  in  front ++ ((key, newValue) : back)
