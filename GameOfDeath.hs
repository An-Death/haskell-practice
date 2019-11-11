module GameOfDeath where

import Data.Either
import Control.Monad.Except


-- https://stepik.org/lesson/38580/step/6?unit=20505

data Tile = Floor | Chasm | Snake
  deriving Show

data DeathReason = Fallen | Poisoned
  deriving (Eq, Show)


type Point = (Integer, Integer)
type GameMap = Point -> Tile
type StepsCount = Int

-- | Test MAP
map1 :: GameMap
map1 (2, 2) = Snake
map1 (4, 1) = Snake
map1 (x, y)
  | 0 < x && x < 5 && 0 < y && y < 5 = Floor
  | otherwise                        = Chasm

--  | 0 1 2 3 4 5
-- --------------
-- 0| o o o o o o
-- 1| o       s o
-- 2| o   s     o
-- 3| o         o
-- 4| o         o
-- 5| o o o o o o


-- | List of all posibility ends of game with current count of steps
moves :: GameMap -> StepsCount  -> Point -> [Either DeathReason Point]
moves m n p = runExceptT $ foldr (>=>) pure (replicate n (ExceptT . move)) p
  where
    steps :: Point -> [Point]
    steps (x, y) = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

    stepOn :: Point -> Either DeathReason Point
    stepOn p = case m p of
      Floor -> Right p
      Chasm -> Left Fallen
      Snake -> Left Poisoned

    move :: Point -> [Either DeathReason Point]
    move p = stepOn <$> steps p

-- | Show you count of possible death by your choice 
-- >>> waysToDie Poisoned map1 1 (4,2)
-- 1 
-- >>> waysToDie Poisoned map1 2 (4,2)
-- 2 
-- >>> waysToDie Poisoned map1 3 (4,2)
-- 5
-- >>> waysToDie Poisoned map1 4 (4,2)
-- 13
waysToDie :: DeathReason -> GameMap -> StepsCount -> Point -> Int
waysToDie reason map' steps point  = length . filter (==reason) . lefts $ possibleMoves 
  where
      possibleMoves = moves map' steps point