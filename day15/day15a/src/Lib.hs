{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib where

import           Control.Lens
import           Control.Monad
import           Control.Monad.State            ( execState
                                                , get
                                                , MonadState
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

data Entity = Wall | Cavern | Unit UnitAttr
   deriving (Eq, Show, Generic, Ord)

data UnitAttr = UnitAttr
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
makeLenses ''UnitAttr
makeLenses ''Cell
makeLenses ''BattleState

makePrisms ''Entity

instance Hashable UnitType
instance Hashable UnitAttr
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
      finalState = execState runGame battleState
  in  calculateResult finalState

runGame :: (MonadState BattleState m) => m ()
runGame = do
  status <- runTurn
  case status of
    StillGoing -> runGame
    Finished   -> return ()

calculateResult :: BattleState -> Int
calculateResult state = (state ^. battleRound)
  * V.foldr accumHealth 0 (state ^. battleMap)
 where
  accumHealth (Cell (Unit (UnitAttr _ h _)) _) a = a + h
  accumHealth _ a = a

runTurn :: (MonadState BattleState m) => m Status
runTurn = do
  mcurrentUnit <- findNextUnit
  case mcurrentUnit of
    Just currentUnit -> do
      targets <- getSortedTargets currentUnit
      case targets of
        [] -> do
          state <- get
          trace (showMap (state ^. mapWidth) (state ^. battleMap))
                return
                Finished
        _ -> do
          newUnit@(Cell _ (x, y)) <- moveUnit currentUnit targets
          attack newUnit
          newIndex <- getIndex x y
          battleMap . ix newIndex %= markMoved
          runTurn
    Nothing -> do
      battleRound += 1
      battleMap %= resetMovedAll
      state <- get
      trace (showMap (state ^. mapWidth) (state ^. battleMap)) return StillGoing

resetMovedAll :: Vector Cell -> Vector Cell
resetMovedAll = V.map resetMoved

resetMoved :: Cell -> Cell
resetMoved cell = cell & cellEntity . _Unit . unitHasMoved .~ False

markMoved :: Cell -> Cell
markMoved cell = cell & cellEntity . _Unit . unitHasMoved .~ True

getSortedTargets :: (MonadState BattleState m) => Cell -> m [Cell]
getSortedTargets orig = do
  possibleTargets <- getPossibleTargets orig
  return $ sortBy compDistance $ V.toList possibleTargets
  where compDistance c1 c2 = distance orig c1 `compare` distance orig c2

getPossibleTargets :: (MonadState BattleState m) => Cell -> m (Vector Cell)
getPossibleTargets (Cell (Unit (UnitAttr Elf _ _)) _) = do
  state <- get
  return $ V.filter
    (\(Cell ent _) -> case ent of
      Unit (UnitAttr Goblin _ _) -> True
      _                          -> False
    )
    (state ^. battleMap)
getPossibleTargets (Cell (Unit (UnitAttr Goblin _ _)) _) = do
  state <- get
  return $ V.filter
    (\(Cell ent _) -> case ent of
      Unit (UnitAttr Elf _ _) -> True
      _                       -> False
    )
    (state ^. battleMap)
getPossibleTargets _ = error "No targets for non-units"

showMap :: Int -> Map -> String
showMap width m =
  concat
    $ mapi (\i c -> (if i `mod` width == 0 then "\n" else "") ++ [cellToChar c])
    $ V.toList m

findNextUnit :: (MonadState BattleState m) => m (Maybe Cell)
findNextUnit = do
  state <- get
  return
    $  V.find
         (\(Cell ent _) -> case ent of
           Unit (UnitAttr _ _ False) -> True
           _                         -> False
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
charToCell x y 'G' = Cell (Unit (UnitAttr Goblin initialHealth False)) (x, y)
charToCell x y 'E' = Cell (Unit (UnitAttr Elf initialHealth False)) (x, y)
charToCell _ _ _   = error "Invalid character in input string"

cellToChar :: Cell -> Char
cellToChar (Cell Wall                         _) = '#'
cellToChar (Cell Cavern                       _) = '.'
cellToChar (Cell (Unit (UnitAttr Goblin _ _)) _) = 'G'
cellToChar (Cell (Unit (UnitAttr Elf    _ _)) _) = 'E'

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

moveUnit :: (MonadState BattleState m) => Cell -> [Cell] -> m Cell
moveUnit unit []      = return unit
moveUnit unit targets = if any (isNeighbor unit) targets
  then return unit
  else do
    possibleDests' <- mapM cavernNeighbors targets
    let possibleDests = concatMap H.toList possibleDests'
    let mclosestDest = if null possibleDests
          then Nothing
          else Just (foldl1' closerNeighbor $ sort possibleDests)
    case mclosestDest of
      Just closestDest -> do
        moves <- aStarM cavernNeighbors
                        distanceM
                        (distanceM closestDest)
                        (return . (==) closestDest)
                        (return unit)
        let nextMove = trace (show moves) head <$> moves
        case nextMove of
          Just (Cell Cavern (x, y)) -> do
            let (Cell u (oldX, oldY)) = unit
            newIndex <- getIndex x y
            oldIndex <- getIndex oldX oldY
            let newUnit = Cell u (x, y)
            battleMap . ix newIndex .= newUnit
            battleMap . ix oldIndex .= Cell Cavern (oldX, oldY)
            return newUnit
          _ -> moveUnit unit $ tail targets
      Nothing -> moveUnit unit $ tail targets
  where closerNeighbor c n = if distance unit n < distance unit c then n else c

attack :: (MonadState BattleState m) => Cell -> m ()
attack c@(Cell (Unit (UnitAttr uType _ _)) _) = do
  ns <- filter isUnit . H.toList <$> neighbors c
  let
    enemies =
      filter (\(Cell (Unit (UnitAttr nType _ _)) _) -> uType /= nType) ns
    sortedEnemies = sortBy
      (\(Cell (Unit (UnitAttr _ h1 _)) _) (Cell (Unit (UnitAttr _ h2 _)) _) ->
        h1 `compare` h2
      )
      enemies
    menemy = if null sortedEnemies then Nothing else Just (head sortedEnemies)
  case menemy of
    Just (Cell Unit{} (x, y)) -> do
      enemyIndex <- getIndex x y
      battleMap . ix enemyIndex . cellEntity . _Unit . unitHealth -= attackPower
      state <- get
      when
          ((state ^? battleMap . ix enemyIndex . cellEntity . _Unit . unitHealth
           )
          <= Just 0
          )
        $  battleMap
        .  ix enemyIndex
        .  cellEntity
        .= Cavern
    _ -> return ()
attack _ = error "Only units can attack"
