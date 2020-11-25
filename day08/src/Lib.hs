module Lib where

data Node = Node Header [Node] [Metadata]
    deriving (Eq, Show)

type Metadata = Int

data Header = Header ChildCount MetadataCount
    deriving (Eq, Show)

type ChildCount = Int
type MetadataCount = Int

headerLength :: Int
headerLength = 2

sumMetadata :: [Node] -> Int
sumMetadata =
    foldr
        ( \n t -> case n of
            Node _ children ns -> t + sum ns + sumMetadata children
        )
        0

nodeValue :: Maybe Node -> Int
nodeValue node = case node of
    Nothing -> 0
    Just (Node _ [] metadatas) -> sum metadatas
    Just (Node _ children metadatas) ->
        sum $ map (nodeValue . getNode children) metadatas

getNode :: [Node] -> Int -> Maybe Node
getNode nodes i =
    if i > length nodes || i <= 0
        then Nothing
        else Just $ nodes !! (i - 1)

parseNodes :: [Int] -> [Node]
parseNodes [] = []
parseNodes (childCount : metadataCount : rest) =
    parseNodes'
        childCount
        metadataCount
        1
        rest
  where
    parseNodes' :: Int -> Int -> Int -> [Int] -> [Node]
    parseNodes' cc mc childCounter xs =
        let nodeLength = lengthOfNode $ cc : mc : xs
         in Node
                (Header cc mc)
                ( if cc == 0
                    then []
                    else parseNodes' (head xs) (xs !! 1) cc (drop 2 xs)
                )
                (takeLast mc $ take (nodeLength - headerLength) xs) :
            if childCounter > 1
                then
                    let (nextCc : nextMc : nextRest) =
                            drop (nodeLength - headerLength) xs
                     in parseNodes' nextCc nextMc (childCounter - 1) nextRest
                else []
parseNodes _ = []

takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

inputToInts :: String -> [Int]
inputToInts = map (read :: String -> Int) . words

lengthOfNode :: [Int] -> Int
lengthOfNode (childCount : metadataCount : xs) =
    lengthOfNode'
        childCount
        metadataCount
        xs
  where
    lengthOfNode' :: Int -> Int -> [Int] -> Int
    lengthOfNode' _ _ [] = 0
    lengthOfNode' 0 mc _ = headerLength + mc
    lengthOfNode' cc mc ys =
        let childNodeLength = lengthOfNode ys
         in childNodeLength + lengthOfNode' (cc - 1) mc (drop childNodeLength ys)
lengthOfNode _ = 0
