module Lib where

import Control.Applicative (liftA2)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Text.Regex.TDFA ((=~))

type Id = Int

data Inch = None | Single Id | Many
    deriving (Show)

data Cloth = Cloth Int (Vector Inch)
    deriving (Show)

data Claim = Claim Id Int Int Int Int
    deriving (Show)

parseInput :: String -> ([Claim], IntSet)
parseInput input =
    foldr
        ( \line (claims, ids) ->
            let (claim, ids') = parseLine line ids
             in (claim : claims, ids')
        )
        ([], IS.empty)
        $ lines input

parseLine :: String -> IntSet -> (Claim, IntSet)
parseLine line ids =
    let (_, _, _, [claimId, x, y, w, h]) =
            line =~ regex :: (String, String, String, [String])
        intId = read claimId
     in ( Claim intId (read x) (read y) (read w) (read h)
        , IS.insert intId ids
        )
  where
    regex = "#([0-9]+) @ ([0-9]+),([0-9]+): ([0-9]+)x([0-9]+)"

makeCloth :: Int -> Cloth
makeCloth size = Cloth size $ V.replicate (size * size) None

incrementClaims :: Claim -> Cloth -> IntSet -> (Cloth, IntSet)
incrementClaims (Claim claimId x y width height) (Cloth clothWidth cloth) ids =
    let coords =
            liftA2
                (\x' y' -> y' * clothWidth + x')
                [x .. width + x - 1]
                [y .. height + y - 1]
        ids' =
            if any (\i -> let inch = cloth ! i in not $ isNone inch) coords
                then IS.delete claimId ids
                else ids
        newIds =
            foldr
                ( \i new -> case cloth ! i of
                    Single claimId' -> IS.delete claimId' new
                    _ -> new
                )
                ids'
                coords
        modifications = zip coords (repeat claimId)
        updatedCloth =
            V.accum
                ( \oldId newId -> case oldId of
                    None -> Single newId
                    _ -> Many
                )
                cloth
                modifications
     in (Cloth clothWidth updatedCloth, newIds)

isMany :: Inch -> Bool
isMany inch = case inch of
    Many -> True
    _ -> False

isNone :: Inch -> Bool
isNone inch = case inch of
    None -> True
    _ -> False

countOverlappingClaims :: [Claim] -> IntSet -> (Int, Id)
countOverlappingClaims claims ids =
    let cloth = makeCloth 1000
        (Cloth _ cloth', nonOverlappingIds) =
            foldr
                ( \claim (cloth'', ids') ->
                    incrementClaims claim cloth'' ids'
                )
                (cloth, ids)
                claims
     in (length $ V.filter isMany cloth', IS.findMin nonOverlappingIds)
