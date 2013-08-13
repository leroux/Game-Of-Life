module GameOfLife where

import           Data.List
import qualified Data.Map as M
import           Data.Maybe

-- Cell
data Cell = Cell {cellState :: CellState, position :: Position}
data CellState = Dead | Alive
type Position = (Int, Int)

-- Grid
type Grid = M.Map Position Cell

instance Show CellState where
  show Dead  = " "
  show Alive = "X"

instance Show Cell where
  show = show . cellState

getNeighbors :: Cell -> Grid -> [Cell]
getNeighbors c g = catMaybes [M.lookup p g | p <- surrounding $ position c] where
  surrounding (x, y) = delete (x, y) [ (m, n) | m <- bounds x, n <- bounds y] where
    bounds x' = [x' - 1, x, x' + 1]

step :: Cell -> Grid -> Cell
step c g = case cellState c of
             Alive -> stepAlive $ length $ getNeighbors c g
             _     -> stepDead $ length $ getNeighbors c g
  where
  p = position c
  stepAlive n
    | n < 2      = Cell Dead p
    | n == 3     = c
    | otherwise  = Cell Dead p
  stepDead n
    | n > 3      = Cell Alive p
    | otherwise  = c
