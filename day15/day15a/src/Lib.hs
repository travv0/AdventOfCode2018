{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import           Control.Lens
import           Control.Monad.State
import           Data.Graph.AStar               ( aStarM )
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

data Entity = Wall | Cavern | Goblin Health | Elf Health
  deriving (Eq, Show, Generic)

type Health = Int

type Map = Vector Cell

data Status = StillGoing | Finished

data BattleState = BattleState
  { _mapWidth :: Int
  , _battleRound :: Int
  , _battleMap :: Map
  } deriving (Eq, Show, Generic)
makeLenses ''BattleState

instance Hashable Entity
instance Hashable Cell

initialHealth :: Health
initialHealth = 200

attackPower :: Int
attackPower = 3

runGame :: String -> Integer
runGame s =
  let width       = (length . head . lines) s
      battleState = BattleState {_mapWidth = width, _battleMap = parseInput s}
  in  do
        let finalState = flip execState battleState runTurn
        calculateResult finalState


calculateResult = undefined

runTurn :: (MonadState BattleState m) => m Status
runTurn = do
  currentUnit     <- findNextUnit
  possibleTargets <- getPossibleTargets currentUnit
  return StillGoing

getPossibleTargets :: (MonadState BattleState m) => Cell -> m (Vector Cell)
getPossibleTargets = undefined

findNextUnit :: (MonadState BattleState m) => m Cell
findNextUnit = undefined

parseInput :: String -> Map
parseInput = V.fromList . concat . mapi (\y -> mapi (`charToCell` y)) . lines

mapi :: (Int -> a -> b) -> [a] -> [b]
mapi f = zipWith f [0 ..]

charToCell :: Int -> Int -> Char -> Cell
charToCell x y '#' = Cell Wall (x, y)
charToCell x y '.' = Cell Cavern (x, y)
charToCell x y 'G' = Cell (Goblin initialHealth) (x, y)
charToCell x y 'E' = Cell (Elf initialHealth) (x, y)
charToCell _ _ _   = error "Invalid character in input string"

neighbors :: (MonadState BattleState m) => Cell -> m (HashSet Cell)
neighbors (Cell _ (x, y)) = do
  ns <- sequence
    [getCell x (y - 1), getCell (x - 1) y, getCell (x + 1) y, getCell x (y + 1)]
  return $ H.fromList $ filter isCavern $ catMaybes ns

isCavern :: Cell -> Bool
isCavern (Cell Cavern _) = True
isCavern _               = False

getCell :: (MonadState BattleState m) => Int -> Int -> m (Maybe Cell)
getCell x y = do
  m <- get
  return $ (m ^. battleMap) !? (x + y * (m ^. mapWidth))

distance :: Cell -> Cell -> Int
distance (Cell _ (x1, y1)) (Cell _ (x2, y2)) = abs (x2 - x1) + abs (y2 - y1)
