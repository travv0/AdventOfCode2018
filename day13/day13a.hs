module Main where

import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V

data Direction = U | R | D | L
  deriving (Eq, Show)

data StraightTrack = Vertical | Horizontal
  deriving (Eq, Show)
data CurvedTrack = FSlash | BSlash
  deriving (Eq, Show)
data Track = StraightTrack StraightTrack
           | CurvedTrack CurvedTrack
           | Intersection
  deriving (Eq, Show)

data Place = Track Track | Cart Direction Track
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
charToPlace '^'  = Just $ Cart U $ StraightTrack Vertical
charToPlace '>'  = Just $ Cart R $ StraightTrack Horizontal
charToPlace 'v'  = Just $ Cart D $ StraightTrack Vertical
charToPlace '<'  = Just $ Cart L $ StraightTrack Horizontal
charToPlace _    = Nothing
