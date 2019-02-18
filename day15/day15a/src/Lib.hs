module Lib
  ( runGame
  )
where

import           Control.Monad.State
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V

data Cell = Wall | Cavern | Goblin | Elf
  deriving (Eq, Show)

type Map = Vector (Vector Cell)

runGame :: String -> Integer
runGame = undefined

parseInput :: String -> Map
parseInput = V.fromList . map (V.fromList . map charToCell) . lines

charToCell :: Char -> Cell
charToCell '#' = Wall
charToCell '.' = Cavern
charToCell 'G' = Goblin
charToCell 'E' = Elf
charToCell _   = error "Invalid character in input string"
