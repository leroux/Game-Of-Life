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

gridToList :: Grid -> [[Cell]]
gridToList = transpose . chunksOf 50 . map snd . M.toAscList . M.filterWithKey inShownGrid
  where inShownGrid (x, y) _ = x `elem` [1..50] && y `elem` [1..100]

instance Show Grid where
  show = unlines . map (intersperse ' ' . map (head . show)) . gridToList

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
insertStructureAt :: Position -> Structure -> Grid -> Grid
insertStructureAt p s = insertStructureWith Alive $ addToPosition s p

isDead :: Grid -> Bool
isDead = M.null . M.filter (== Alive)

isAlive :: Grid -> Bool
isAlive = not . isDead

----
-- Cell Interactions
----
-- Number of alive neighbors of a position.
neighborsAlive :: Position -> Grid -> Int
neighborsAlive p g = length $ filter (== Alive) $ getNeighbors p g

getNeighbors :: Position -> Grid -> [Cell]
getNeighbors p g = catMaybes [M.lookup n g | n <- surrounding p]

unbound :: Structure -> Grid -> Structure
unbound s g = map around s
  where around (x, y) = (x `mod` mx, y `mod` my)
        (mx, my) = fst $ M.findMax g

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
    | n `elem` [2, 3] = Alive
    | otherwise       = Dead
  stepDead n
    | n == 3    = Alive
    | otherwise = Dead 

----
-- Generations
----
-- Next generation.
tick :: Grid -> Grid
tick g = M.mapWithKey (\k v -> step k v g) g

----
-- Seeds
----
blinker :: Structure
blinker = zip (repeat 0) [0..3]

addToPosition :: Structure -> Position -> Structure
addToPosition s (x, y) = map (\(a, b) -> (x + a, y + b)) s

glider :: Structure
glider = [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]

brilliant :: Structure
brilliant = [(1, 0), (3, 1), (0, 2), (1, 2), (4, 2), (5, 2), (6, 2)]

line :: Structure
line = zip [1..10] (repeat 0)

infinite :: Structure
infinite = [(0, 0), (1, 0), (2, 0), (4, 0),
            (0, 1),
            (3, 2), (4, 2),
            (1, 3), (2, 3), (4, 3),
            (0, 4), (2, 4), (4, 4)]

provisionGrid :: Grid
provisionGrid = insertStructureAt (25, 25) infinite (deadGrid 50 50)
