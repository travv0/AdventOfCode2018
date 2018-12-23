module Main where

import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V

data Direction = U | R | D | L
  deriving (Eq, Show)
data TurnDirection = TurnLeft | GoStraight | TurnRight
  deriving (Eq, Show)

data StraightTrack = Vertical | Horizontal
  deriving (Eq, Show)
data CurvedTrack = FSlash | BSlash
  deriving (Eq, Show)
data Track = StraightTrack StraightTrack
           | CurvedTrack CurvedTrack
           | Intersection
  deriving (Eq, Show)

type TrackBeneath = Track
data Place = Track Track | Cart Direction TurnDirection TrackBeneath
  deriving (Eq, Show)

main :: IO ()
main = interact (show . V.fromList . map parseLine . lines)

parseLine :: String -> Vector (Maybe Place)
parseLine = V.fromList . map charToPlace

charToPlace :: Char -> Maybe Place
charToPlace '-'  = Just $ Track $ StraightTrack Horizontal
charToPlace '|'  = Just $ Track $ StraightTrack Vertical
charToPlace '\\' = Just $ Track $ CurvedTrack BSlash
charToPlace '/'  = Just $ Track $ CurvedTrack FSlash
charToPlace '+'  = Just $ Track Intersection
charToPlace '^'  = Just $ Cart U TurnLeft $ StraightTrack Vertical
charToPlace '>'  = Just $ Cart R TurnLeft $ StraightTrack Horizontal
charToPlace 'v'  = Just $ Cart D TurnLeft $ StraightTrack Vertical
charToPlace '<'  = Just $ Cart L TurnLeft $ StraightTrack Horizontal
charToPlace _    = Nothing
