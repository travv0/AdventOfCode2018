module Lib where

import Control.Applicative (liftA2)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Text.Regex.TDFA ((=~))

data Cloth = Cloth Int (Vector Int)
    deriving (Show)

data Claim = Claim Int Int Int Int
    deriving (Show)

parseInput :: String -> [Claim]
parseInput input = map parseLine $ lines input

parseLine :: String -> Claim
parseLine line =
    let (_, _, _, [x, y, w, h]) =
            line =~ regex :: (String, String, String, [String])
     in Claim (read x) (read y) (read w) (read h)
  where
    regex = "#[0-9]+ @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)"

makeCloth :: Int -> Cloth
makeCloth size = Cloth size $ V.replicate (size * size) 0

incrementClaims :: Claim -> Cloth -> Int -> (Cloth, Int)
incrementClaims (Claim x y width height) (Cloth clothWidth cloth) overlaps =
    let coords =
            liftA2
                (\x y -> y * clothWidth + x)
                [x .. width + x - 1]
                [y .. height + y - 1]
        modifications = zip coords (repeat 1)
        updatedCloth = V.accum (+) cloth modifications
        newOverlaps = length $ filter (\i -> (updatedCloth ! i) == 2) coords
     in (Cloth clothWidth updatedCloth, overlaps + newOverlaps)

countOverlappingClaims :: [Claim] -> Int
countOverlappingClaims claims =
    let cloth = makeCloth 1000
        (_, overlaps) =
            foldr
                ( \claim (cloth', overlaps) ->
                    incrementClaims claim cloth' overlaps
                )
                (cloth, 0)
                claims
     in overlaps
