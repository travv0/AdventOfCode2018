module Main where

import           Debug.Trace

data Node = Node Header [Node] [Metadata]
  deriving Show

type Metadata = Int

data Header = Header ChildCount MetadataCount
  deriving Show

type ChildCount = Int
type MetadataCount = Int

testInput = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

main :: IO ()
main = print "HI"

sumMetadata :: String -> Int
sumMetadata s = sum $ map (read :: String -> Int) $ words s

parseNode :: [Int] -> [Node]
parseNode [] = []
parseNode (childCount : metadataCount : rest) = parseNode' childCount
                                                           metadataCount
                                                           rest
 where
  parseNode' :: Int -> Int -> [Int] -> [Node]
  parseNode' _ _  [] = []
  parseNode' 0 mc xs = [Node (Header 0 mc) [] (take mc xs)]
  parseNode' cc mc xs =
    let childNodeLength = lengthOfNode xs
        childNode       = parseNode $ drop childNodeLength xs
    in  [ Node (Header cc mc)
               (childNode ++ parseNode' (cc - 1) mc (drop childNodeLength xs))
               []
        ]



inputToInts :: String -> [Int]
inputToInts = map (read :: String -> Int) . words

headerLength = 2

lengthOfNode :: [Int] -> Int
lengthOfNode (childCount : metadataCount : xs) = lengthOfNode' childCount
                                                               metadataCount
                                                               xs
 where
  lengthOfNode' :: Int -> Int -> [Int] -> Int
  lengthOfNode' _ _  [] = 0
  lengthOfNode' 0 mc _  = headerLength + mc
  lengthOfNode' cc mc xs =
    let childNodeLength = lengthOfNode xs
    in  childNodeLength + lengthOfNode' (cc - 1) mc (drop childNodeLength xs)
