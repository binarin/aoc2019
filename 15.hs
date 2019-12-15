{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import qualified Data.PQueue.Min as Q
import Control.Lens hiding (Empty)
import qualified Data.Map as M
import Control.Lens.TH
import Data.IORef (newIORef)
import qualified Data.Set as S

import IntCode

type Coord = Int
type Point = (Int, Int)

data Cell = Empty | Wall | OxygenTank | Unknown deriving (Show, Eq)
data Direction = North | East | South | West deriving (Show, Eq)

type Map = M.Map Point Cell

data ExplorationStatus = Moving [Direction] | Exploring deriving (Show)

data PlotState = PlotState { _plotStatePositionL :: Point
                           , _plotStateMapL :: Map
                           , _plotStateQueueL :: S.Set Point
                           } deriving (Show)

makeFields ''PlotState

getPlotCell :: PlotState -> Point -> Cell
getPlotCell plot (x, y) =
  case M.lookup (x, y) (plot^.mapL) of
    Nothing -> Unknown
    Just cell -> cell


plotOnArrival :: PlotState -> PlotState
plotOnArrival old = new
  where
    thisC = old^.positionL
    upC = thisC & _2 %~ pred
    downC = thisC & _2 %~ succ
    leftC = thisC & _1 %~ pred
    rightC = thisC & _1 %~ succ
    unknownAdjacent = filter (\c -> getPlotCell old c == Unknown) [upC, downC, leftC, rightC]
    new = old & mapL %~ (M.insert thisC Empty)
              & queueL %~ (S.union (S.fromList unknownAdjacent))


adjacentCells :: Point -> [Point]
adjacentCells thisC =
  let upC = thisC & _2 %~ pred
      downC = thisC & _2 %~ succ
      leftC = thisC & _1 %~ pred
      rightC = thisC & _1 %~ succ
  in [upC, downC, leftC, rightC]

advance :: Direction -> Point -> Point
advance North = over _2 pred
advance South = over _2 succ
advance West = over _1 pred
advance East = over _1 succ

-- data SearchState = SearchState { _searchStateSeenL :: S.Set Point
--                                , _searchStateCostsL :: M.Map Point (Int, [Direction])
--                                , _searchStateQueueL :: Q.MinQueue (Int, Point

-- searchPath :: PlotState -> Point -> Point -> [Direction]
-- searchPath plot f t = undefined
--   where
--     go :: SearchNode -> S.Set Point -> M.Map Point SearchNode -> Q.MinQueue (Int, Point, [Direction]) -> [Direction]
--     go cost visited costs queue =
--       let ((curCost, cur, curDirs), queue') = Q.deleteFindMin queue
--           visit dir
--       in undefined

plot :: IO Map
plot = do
  let state = plotOnArrival (PlotState (0, 0) M.empty S.empty)
  stateRef <- newIORef state

  print state


  let haltAction = undefined
      inputAction = undefined
      outputAction = undefined

  let opcodes = mkOpcodesTable (pureOpcodes ++ mkIoOpcodes haltAction inputAction outputAction)
  vm <- readProgram "15.txt" >>= flip makeRunnable opcodes

  runIntcode vm runVM

  pure M.empty

main :: IO ()
main = do
  knownMap <- plot
  pure ()
