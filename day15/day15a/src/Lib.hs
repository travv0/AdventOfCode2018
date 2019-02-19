{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import           Control.Lens                   ( (.=)
                                                , (^.)
                                                , ix
                                                , makeLenses
                                                )
import           Control.Monad.State            ( execState
                                                , get
                                                , MonadState
                                                )
import           Data.Graph.AStar               ( aStarM )
import           Data.HashSet                   ( HashSet )
import           Data.Hashable
import           Data.Maybe                     ( catMaybes )
import           Data.Vector                    ( Vector
                                                , (!?)
                                                )
import           GHC.Generics                   ( Generic )
import qualified Data.HashSet                  as H
import qualified Data.Vector                   as V

data Cell = Cell Entity Pos
  deriving (Eq, Show, Generic)

instance Ord Cell where
  (Cell _ (x1, y1)) `compare` (Cell _ (x2, y2)) =
    y1 `compare` y2 <> x1 `compare` x2

type Pos = (Int, Int)

data Entity = Wall | Cavern | Unit UnitType Health Bool
  deriving (Eq, Show, Generic, Ord)

data UnitType = Goblin | Elf
  deriving (Eq, Show, Generic, Ord)

type Health = Int

type Map = Vector Cell

data Status = StillGoing | Finished

data BattleState = BattleState
  { _mapWidth :: Int
  , _battleRound :: Int
  , _battleMap :: Map
  } deriving (Eq, Show, Generic)
makeLenses ''BattleState

instance Hashable UnitType
instance Hashable Entity
instance Hashable Cell

initialHealth :: Health
initialHealth = 200

attackPower :: Int
attackPower = 3

runGame :: String -> Integer
runGame s =
  let width       = (length . head . lines) s
      battleState = BattleState
        { _mapWidth    = width
        , _battleMap   = parseInput s
        , _battleRound = 0
        }
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
charToCell x y 'G' = Cell (Unit Goblin initialHealth False) (x, y)
charToCell x y 'E' = Cell (Unit Elf initialHealth False) (x, y)
charToCell _ _ _   = error "Invalid character in input string"

neighbors :: (MonadState BattleState m) => Cell -> m (HashSet Cell)
neighbors (Cell _ (x, y)) = do
  ns <- sequence
    [getCell x (y - 1), getCell (x - 1) y, getCell (x + 1) y, getCell x (y + 1)]
  return $ H.fromList $ filter isCavern $ catMaybes ns

isCavern :: Cell -> Bool
isCavern (Cell Cavern _) = True
isCavern _               = False

getIndex :: (MonadState BattleState m) => Int -> Int -> m Int
getIndex x y = do
  state <- get
  return $ x + y * (state ^. mapWidth)

getCell :: (MonadState BattleState m) => Int -> Int -> m (Maybe Cell)
getCell x y = do
  state <- get
  index <- getIndex x y
  return $ (state ^. battleMap) !? index

distance :: Cell -> Cell -> Int
distance (Cell _ (x1, y1)) (Cell _ (x2, y2)) = abs (x2 - x1) + abs (y2 - y1)

distanceM :: Monad m => Cell -> Cell -> m Int
distanceM c1 c2 = do
  let r = distance c1 c2
  return r

moveUnit :: (MonadState BattleState m) => Cell -> Cell -> m ()
moveUnit unit dest = do
  destNeighbors <- H.toList <$> neighbors dest
  let closestNeighbor = foldr1 closerNeighbor destNeighbors
  nextMove <- fmap head <$> aStarM neighbors
                                   distanceM
                                   (distanceM dest)
                                   (return . (==) closestNeighbor)
                                   (return unit)
  case nextMove of
    Just (Cell Cavern (x, y)) -> do
      let (Cell (Unit u h _) (oldX, oldY)) = unit
      newIndex <- getIndex x y
      oldIndex <- getIndex oldX oldY
      battleMap . ix newIndex .= Cell (Unit u h True) (x, y)
      battleMap . ix oldIndex .= Cell Cavern (oldX, oldY)
    _ -> return ()
  where closerNeighbor n c = if distance unit n < distance unit c then n else c
