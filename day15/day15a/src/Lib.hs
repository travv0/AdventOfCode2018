{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( runGame
  )
where

import           Control.Monad.State
import           Data.HashSet                   ( HashSet )
import           Data.Hashable
import           Data.Maybe                     ( catMaybes )
import           Data.Vector                    ( Vector
                                                , (!?)
                                                )
import           GHC.Generics
import qualified Data.HashSet                  as H
import qualified Data.Vector                   as V

data Cell = Cell Entity Pos
  deriving (Eq, Show, Generic)

type Pos = (Int, Int)

data Entity = Wall | Cavern | Goblin | Elf
  deriving (Eq, Show, Generic)

type Map = Vector (Vector Cell)

instance Hashable Entity
instance Hashable Cell

runGame :: String -> Integer
runGame = undefined

parseInput :: String -> Map
parseInput =
  V.fromList . mapi (\y -> V.fromList . mapi (`charToEntity` y)) . lines

mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f = zipWith f [0 ..]

charToEntity :: Int -> Int -> Char -> Cell
charToEntity x y '#' = Cell Wall (x, y)
charToEntity x y '.' = Cell Cavern (x, y)
charToEntity x y 'G' = Cell Goblin (x, y)
charToEntity x y 'E' = Cell Elf (x, y)
charToEntity _ _ _   = error "Invalid character in input string"

neighbors :: (MonadState Map m) => Cell -> m (HashSet Cell)
neighbors (Cell _ (x, y)) = do
  ns <- sequence
    [getCell x (y - 1), getCell (x - 1) y, getCell (x + 1) y, getCell x (y + 1)]
  return $ H.fromList $ catMaybes ns

getCell :: (MonadState Map m) => Int -> Int -> m (Maybe Cell)
getCell x y = do
  m <- get
  return $ (m !? y) >>= (!? x)

distance :: Cell -> Cell -> Int
distance (Cell _ (x1, y1)) (Cell _ (x2, y2)) = abs (x2 - x1) + abs (y2 - y1)
