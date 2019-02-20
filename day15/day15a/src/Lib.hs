{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State            ( execState
                                                , get
                                                , MonadState
                                                , unless
                                                )
import           Data.Graph.AStar               ( aStarM )
import           Data.HashSet                   ( HashSet )
import           Data.Hashable
import           Data.List
import           Data.Maybe                     ( catMaybes )
import           Data.Vector                    ( Vector
                                                , (!?)
                                                )
import           GHC.Generics                   ( Generic )
import qualified Data.HashSet                  as H
import qualified Data.Vector                   as V

import           Debug.Trace

data Cell = Cell
  { _cellEntity :: Entity
  , _cellPos :: Pos
  } deriving (Eq, Show, Generic)

instance Ord Cell where
  (Cell _ (x1, y1)) `compare` (Cell _ (x2, y2)) =
    y1 `compare` y2 <> x1 `compare` x2

type Pos = (Int, Int)

data Entity = Wall | Cavern | Unit
  { _unitType :: UnitType
  , _unitHealth :: Health
  , _unitHasMoved :: Bool
  } deriving (Eq, Show, Generic, Ord)

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

makeLenses ''Entity
makeLenses ''Cell
makeLenses ''BattleState

makePrisms ''Entity

instance Hashable UnitType
instance Hashable Entity
instance Hashable Cell

initialHealth :: Health
initialHealth = 200

attackPower :: Int
attackPower = 3

startGame :: String -> Int
startGame s =
  let width       = (length . head . lines) s
      battleState = BattleState
        { _mapWidth    = width
        , _battleMap   = parseInput s
        , _battleRound = 0
        }
  in  do
        let finalState = execState runGame battleState
        trace (show finalState) calculateResult finalState

runGame :: (MonadState BattleState m) => m ()
runGame = do
  status <- runTurn
  case status of
    StillGoing -> runGame
    Finished   -> return ()

calculateResult :: BattleState -> Int
calculateResult state =
  trace
      (show (state ^. battleRound) ++ " " ++ show
        (V.foldr accumHealth 0 (state ^. battleMap))
      )
      (state ^. battleRound)
    * V.foldr accumHealth 0 (state ^. battleMap)
 where
  accumHealth (Cell (Unit _ h _) _) a = a + h
  accumHealth _                     a = a

runTurn :: (MonadState BattleState m) => m Status
runTurn = do
  mcurrentUnit <- findNextUnit
  case mcurrentUnit of
    Just currentUnit -> do
      mclosestTarget <- getClosestTarget currentUnit
      case mclosestTarget of
        Just target -> do
          moveUnit currentUnit target
          attack currentUnit
          runTurn
        Nothing -> return Finished
    Nothing -> do
      battleRound += 1
      battleMap %= resetMovedAll
      return StillGoing

resetMovedAll :: Vector Cell -> Vector Cell
resetMovedAll = V.map resetMoved

resetMoved :: Cell -> Cell
resetMoved (Cell (Unit u h _) p) = Cell (Unit u h False) p
resetMoved c                     = c

getClosestTarget :: (MonadState BattleState m) => Cell -> m (Maybe Cell)
getClosestTarget orig = do
  possibleTargets <- getPossibleTargets orig
  if null possibleTargets
    then return Nothing
    else return $ Just $ foldr1 closerTarget possibleTargets
  where closerTarget c n = if distance orig n < distance orig c then n else c

getPossibleTargets :: (MonadState BattleState m) => Cell -> m (Vector Cell)
getPossibleTargets (Cell (Unit Elf _ _) _) = do
  state <- get
  return $ V.filter
    (\(Cell ent _) -> case ent of
      Unit Goblin _ _ -> True
      _               -> False
    )
    (state ^. battleMap)
getPossibleTargets (Cell (Unit Goblin _ _) _) = do
  state <- get
  return $ V.filter
    (\(Cell ent _) -> case ent of
      Unit Elf _ _ -> True
      _            -> False
    )
    (state ^. battleMap)
getPossibleTargets _ = error "No targets for non-units"

findNextUnit :: (MonadState BattleState m) => m (Maybe Cell)
findNextUnit = do
  state <- get
  return
    $  V.find
         (\(Cell ent _) -> case ent of
           Unit _ _ False -> True
           _              -> False
         )
    $  state
    ^. battleMap

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
  return $ H.fromList $ catMaybes ns

cavernNeighbors :: (MonadState BattleState m) => Cell -> m (HashSet Cell)
cavernNeighbors c = do
  ns <- neighbors c
  return $ H.fromList $ filter isCavern $ H.toList ns

isNeighbor :: Cell -> Cell -> Bool
isNeighbor (Cell _ (x1, y1)) (Cell _ (x2, y2)) =
  (x1 == x2 && (y1 == y2 - 1 || y1 == y2 + 1))
    || (y1 == y2 && (x1 == x2 - 1 || x1 == x2 + 1))

isCavern :: Cell -> Bool
isCavern (Cell Cavern _) = True
isCavern _               = False

isUnit :: Cell -> Bool
isUnit (Cell Unit{} _) = True
isUnit _               = False

getIndex :: (MonadState BattleState m) => Int -> Int -> m Int
getIndex x y = do
  state <- get
  return $ x + y * (state ^. mapWidth)

getCell :: (MonadState BattleState m) => Int -> Int -> m (Maybe Cell)
getCell x y = do
  state <- get
  i     <- getIndex x y
  return $ (state ^. battleMap) !? i

distance :: Cell -> Cell -> Int
distance (Cell _ (x1, y1)) (Cell _ (x2, y2)) = abs (x2 - x1) + abs (y2 - y1)

distanceM :: Monad m => Cell -> Cell -> m Int
distanceM c1 c2 = do
  let r = distance c1 c2
  return r

moveUnit :: (MonadState BattleState m) => Cell -> Cell -> m ()
moveUnit unit@(Cell _ (origX, origY)) dest = unless (isNeighbor unit dest) $ do
  destNeighbors <- H.toList <$> neighbors dest
  let mclosestNeighbor = if null destNeighbors
        then Nothing
        else Just (foldl1' closerNeighbor destNeighbors)
  case mclosestNeighbor of
    Just closestNeighbor -> do
      nextMove <- fmap head <$> aStarM cavernNeighbors
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
        _ -> do
          i <- getIndex origX origY
          battleMap . ix i . cellEntity . _Unit . _3 .= True
    Nothing -> return ()
  where closerNeighbor c n = if distance unit n < distance unit c then n else c

attack :: (MonadState BattleState m) => Cell -> m ()
attack c@(Cell (Unit uType _ _) _) = do
  ns <- filter isUnit . H.toList <$> neighbors c
  let
    enemies       = filter (\(Cell (Unit nType _ _) _) -> uType /= nType) ns
    sortedEnemies = sortBy
      (\(Cell (Unit _ h1 _) _) (Cell (Unit _ h2 _) _) -> h1 `compare` h2)
      enemies
    menemy = if null sortedEnemies then Nothing else Just (head sortedEnemies)
  case menemy of
    Just (Cell Unit{} (x, y)) -> do
      enemyIndex <- getIndex x y
      battleMap . ix enemyIndex . cellEntity . _Unit . _2 -= attackPower
      state <- get
      when
          (  (state ^? battleMap . ix enemyIndex . cellEntity . _Unit . _2)
          <= Just 0
          )
        $  battleMap
        .  ix enemyIndex
        .  cellEntity
        .= Cavern
    _ -> return ()
attack _ = error "Only units can attack"
