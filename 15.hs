{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import Control.Monad.IO.Class
import qualified Data.PQueue.Min as Q
import Control.Lens hiding (Empty)
import qualified Data.Map as M
import Control.Lens.TH
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Set as S
import Graphics.Gloss hiding (Point)

import IntCode

type Coord = Int
type Point = (Int, Int)

data Cell = Wall | Empty | OxygenTank | Unknown deriving (Show, Eq, Enum)
data Direction = North | East | South | West deriving (Show, Eq)

type Map = M.Map Point Cell

data ExplorationStatus = Moving [Direction] | Exploring deriving (Show)

data StackOp = Push Direction Point | Pop Direction deriving (Show)

data PlotState = PlotState { _plotStateMapL :: Map
                           , _plotStateQueueL :: [StackOp]
                           } deriving (Show)

makeFields ''PlotState

getPlotCell :: PlotState -> Point -> Cell
getPlotCell plot (x, y) =
  case M.lookup (x, y) (plot^.mapL) of
    Nothing -> Unknown
    Just cell -> cell

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

allDirections :: [Direction]
allDirections = [North, South, West, East]

dirToCommand :: Direction -> MachineWord
dirToCommand North = 1
dirToCommand South = 2
dirToCommand West = 3
dirToCommand East = 4

inverse :: Direction -> Direction
inverse South = North
inverse North = South
inverse West = East
inverse East = West

plot :: IO Map
plot = do
  let origin = (0, 0)
      initialStackOp dir = Push dir (advance dir origin)
      state = PlotState (M.singleton origin Empty) (initialStackOp <$> allDirections)

  stateRef <- newIORef state

  let haltAction = liftIO $ putStrLn "halted"

      inputAction :: App MachineWord = do
        state <- liftIO $ readIORef stateRef
        case state^.queueL of
          [] -> do
            haltedL .= True
            pure 0
          (Push dir pt:_) -> do
            -- liftIO $ putStrLn $ "Advancing " ++ show dir ++ " to " ++ show pt
            pure $ dirToCommand dir
          (Pop dir:_) -> do
            --liftIO $ putStrLn $ "Backtracking " ++ show dir
            pure $ dirToCommand dir

      outputAction result = do
        state <- liftIO $ readIORef stateRef
        case state^.queueL of
          (Push dir pt:_) -> handlePushResult result dir pt
          (Pop dir:_) -> handlePopResult result

      handlePushResult result dir pt = do
        state <- liftIO $ readIORef stateRef
        case result of
          0 -> liftIO $ do
            -- putStrLn $ "Hit a wall at " ++ show pt
            writeIORef stateRef (state & queueL %~ tail
                                       & mapL %~ (M.insert pt Wall))
          _ -> do
            let cell :: Cell = toEnum $ fromIntegral result
                rollback = Pop (inverse dir)
                possibleDirections = filter (\d -> getPlotCell state (advance d pt) == Unknown) allDirections
                pushes = (\d -> Push d (advance d pt)) <$> possibleDirections
                queue' = pushes ++ [rollback] ++ (tail $ state^.queueL)
                state' = state & queueL .~ queue'
                               & mapL %~ (M.insert pt cell)
            liftIO $ do
              writeIORef stateRef state'

      handlePopResult 0 = error "Failed to backtrack"
      handlePopResult _ = liftIO $ do
        state <- liftIO $ readIORef stateRef
        writeIORef stateRef (state & queueL %~ tail)


  let opcodes = mkOpcodesTable (pureOpcodes ++ mkIoOpcodes haltAction inputAction outputAction)
  vm <- readProgram "15.txt" >>= flip makeRunnable opcodes

  runIntcode vm runVM

  finalState <- readIORef stateRef

  pure $ finalState^.mapL


blockSize :: Int
blockSize = 4

tileAt :: Int -> Int -> Picture -> Picture
tileAt x y pic = Translate (fromIntegral $ x * blockSize) (negate $ fromIntegral $ y * blockSize) $ Scale (fromIntegral blockSize) (fromIntegral blockSize) pic

square :: Picture
square = Polygon [(0, 0), (0, 1), (1, 1), (1, 0)]

cellToPicture :: Cell -> Picture
cellToPicture Empty = Color black square
cellToPicture Wall = Color red square
cellToPicture OxygenTank = Color green square
cellToPicture Unknown = Color blue square

mapToPicture :: Map -> Picture
mapToPicture m = Pictures $
  (\((x, y), cell) -> tileAt x y (cellToPicture cell)) <$> M.toList m


findTank :: Map -> Point
findTank m = fst $ head $ filter (\(_, c) -> c == OxygenTank) (M.toList m)

type ShortestMap = M.Map Point (Int, Cell)

shortestPathRound :: ShortestMap -> ShortestMap
shortestPathRound m =
  let pickLeastRoute a@(_, (aCost, _)) b@(_, (bCost, _))
        | aCost <= bCost = a
        | otherwise = b
      (leastCoord, (leastCost, leastCell)) = foldl1 pickLeastRoute (M.toList m)

      candidates = flip advance leastCoord <$> allDirections

      validTarget c =
        case M.lookup c m of
          Just (_, Empty) -> True
          Just (_, OxygenTank) -> True
          Nothing -> False

      targets = filter validTarget candidates

      applyTarget :: ShortestMap -> Point -> ShortestMap
      applyTarget m c =
        let Just (oldCost, oldCell) = M.lookup c m
            newCost = leastCost + 1
        in if oldCost <= newCost
           then m
           else M.insert c (newCost, oldCell) m

  in if leastCell == OxygenTank
     then m
     else M.delete leastCoord (foldl applyTarget m targets)

mapToShortestMap :: Map -> ShortestMap
mapToShortestMap = M.filter (\(_, c) -> c /= Wall) . M.map (\cell -> (maxBound, cell))

takeWhileUnique :: Eq a => [a] -> [a]
takeWhileUnique [] = []
takeWhileUnique [a] = [a]
takeWhileUnique (a:b:rest)
  | a == b = [a]
  | otherwise = a : takeWhileUnique (b:rest)

oxigenate :: Map -> Map
oxigenate m =
  let currentlyOxygenized = [ pos | (pos, OxygenTank) <- M.toList m]
      targets = concatMap (\c -> flip advance c <$> allDirections) currentlyOxygenized
      validTarget c = case M.lookup c m of
                        Just Empty -> True
                        _ -> False
      validTargets = filter validTarget targets

  in foldl (\m c -> M.insert c OxygenTank m) m validTargets

main :: IO ()
main = do
  knownMap <- plot

  -- display (InWindow "Nice Window" (200, 200) (10, 10)) red (mapToPicture knownMap)
  simulate
    (InWindow "Nice Window" (200, 200) (10, 10))
    red
    50
    knownMap
    mapToPicture
    (\_ _ m -> oxigenate m)

  let spIn = M.insert (0, 0) (0, Empty) $ mapToShortestMap knownMap
      tankCoord = findTank knownMap
      steps = takeWhileUnique $ iterate shortestPathRound spIn

      oxygenSteps = takeWhileUnique $ iterate oxigenate knownMap
      countOxygen = M.foldr (\c a -> if c == OxygenTank then a + 1 else a) 0

  -- sequence $ print . M.size <$> steps
  print $ M.lookup tankCoord (last steps)
  print $ length oxygenSteps - 1
  pure ()
