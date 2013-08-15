{-# LANGUAGE OverlappingInstances, FlexibleInstances #-}

module Game where

import           Data.List (delete, transpose, nub, intersperse)
import           Data.List.Split (chunksOf)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)

----
-- Cell
----
data Cell = Dead | Alive deriving Eq
type Position = (Int, Int)
type Structure = [Position]

instance Show Cell where
  show Dead  = " "
  show Alive = "X"

----
-- Grid
----
type Grid = M.Map Position Cell

instance Show Grid where
  show g = (unlines . map (intersperse ' ' . map (head . show)) . transpose . chunksOf m . map snd . M.toAscList) g where
    m = (fst . fst . M.findMax) g

-- Create grid of size (m * n) and populate with Cell c.
initGrid :: Cell -> Int -> Int -> Grid
initGrid c m n = insertStructureWith c [ (x, y) | x <- [1..m], y <- [1..n] ] M.empty

-- Completely dead grid.
deadGrid :: Int -> Int -> Grid
deadGrid = initGrid Dead

-- Fully alive grid.
aliveGrid :: Int -> Int -> Grid
aliveGrid = initGrid Alive

-- Insert cell structure into grid.
insertStructureWith :: Cell -> Structure -> Grid -> Grid
insertStructureWith c s g = foldr (\k -> M.insert k c) g s
 
-- Insert alive cells structure into grid.
insertStructure :: Structure -> Grid -> Grid
insertStructure = insertStructureWith Alive

isDead :: Grid -> Bool
isDead = M.null . M.filter (== Alive)

isAlive :: Grid -> Bool
isAlive = not . isDead

----
-- Cell Interactions
----
-- Number of alive neighbors of a position.
neighborsAlive :: Position -> Grid -> Int
neighborsAlive p g = length $ filter (== Alive) $ catMaybes [M.lookup n g | n <- surrounding p]

-- Surrounding positions of some position.
-- Should returns a list of 8 Positions.
surrounding :: Position -> [Position]
surrounding (x, y) = nub $ delete (x, y) [ (m, n) | m <- near x, n <- near y] where
  near x' = [x' - 1, x', x' + 1]

-- Next state of a cell based on its environment.
step :: Position -> Cell -> Grid -> Cell
step p c g = let neighbors = neighborsAlive p g in
    case c of
      Alive -> stepAlive neighbors
      _     -> stepDead neighbors
  where
  stepAlive n
    | n < 2     = Dead  -- underpopulation
    | n <= 3    = Alive -- lives, in balance
    | otherwise = Dead  -- overcrowding
  stepDead n
    | n == 3    = Alive -- birth
    | otherwise = Dead  -- Sorry... Life is harsh.

----
-- Generations
----
-- Next generation.
tick :: Grid -> Grid
tick g = M.mapWithKey (\k v -> step k v g) g

----
-- Seeds
----
blinker :: Position -> Structure
blinker (x, y) = zip (repeat x) (take 3 $ iterate succ y)

glider :: Position -> Structure
glider (x, y) = map (\(a, b) -> (x + a, y + b)) glider' where
  glider' = [(1, 1), (2, 2), (0, 3), (1, 3), (2, 3)]

provisionGrid :: Grid
provisionGrid = insertStructure (glider (5, 5)) (deadGrid 50 50)
