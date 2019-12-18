{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
-- {-# LANGUAGE Strict #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.List
import Data.List
import Data.Maybe
import Debug.Trace
import Control.Lens
import Control.Lens.TH
import IntCode
import Control.Monad.IO.Class
import Data.IORef
import Data.Char
import Data.Array.Unboxed

data Direction = North | East | South | West deriving (Show)

type Coord = Int

data Map = Map { _mapWidthL :: Int
               , _mapHeightL :: Int
               , _mapCellsL :: Array Int Bool
               } deriving (Show)

data Robot = Robot { _robotXL :: Int
                   , _robotYL :: Int
                   , _robotDirL :: Direction
                   } deriving (Show)

makeFields ''Map
makeFields ''Robot

hasScaffold :: Char -> Bool
hasScaffold 'X' = False
hasScaffold '.' = False
hasScaffold _ = True

sampleMap :: String
sampleMap = concat [ "..#..........\n"
                   , "..#..........\n"
                   , "#######...###\n"
                   , "#.#...#...#.#\n"
                   , "#############\n"
                   , "..#...#...#..\n"
                   , "..#####...^..\n"
                   ]


sampleMap2 :: String
sampleMap2 = concat [ "#######...#####\n"
                    , "#.....#...#...#\n"
                    , "#.....#...#...#\n"
                    , "......#...#...#\n"
                    , "......#...###.#\n"
                    , "......#.....#.#\n"
                    , "^########...#.#\n"
                    , "......#.#...#.#\n"
                    , "......#########\n"
                    , "........#...#..\n"
                    , "....#########..\n"
                    , "....#...#......\n"
                    , "....#...#......\n"
                    , "....#...#......\n"
                    , "....#####......\n"
                    ]

parseMap :: String -> Map
parseMap input =
  let ls = lines input
      width = length (head ls)
      fullLines = filter ((==width).length) ls
      scaffolds :: [Bool] = concatMap (fmap hasScaffold) fullLines
      height = length fullLines
  in Map width height (listArray (0,width*height-1) scaffolds)

getCell :: Int -> Int -> Map -> Bool
getCell x y m =
  let w = m^.widthL
      h = m^.heightL
      cells = m^.cellsL
      maxX = w - 1
      maxY = h - 1
  in if x > maxX || x < 0 || y > maxY || y < 0
     then False
     else cells ! (x + y * w)

isIntesection :: Map -> Int -> Int -> Bool
isIntesection m x y = and ((\(x, y) -> getCell x y m) <$> adjacent)
  where
    adjacent = [(x, y), (x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

getIntersections :: Map -> [(Int, Int)]
getIntersections m = [ (x, y) | y <- [0..m^.heightL-1], x <- [0..m^.widthL-1], isIntesection m x y ]

data Move = L Int | R Int

instance Show Move where
  show (L d) = "L," ++ show d
  show (R d) = "R," ++ show d


robotX :: String -> Maybe (Int, Direction)
robotX = go 0
  where
    go pos [] = Nothing
    go pos ('^':_) = Just (pos, North)
    go pos ('v':_) = Just (pos, South)
    go pos ('<':_) = Just (pos, West)
    go pos ('>':_) = Just (pos, East)
    go pos (_:cs) = go (pos + 1) cs


findDrone :: String -> Robot
findDrone m =
  let scanlines = lines m
      possiblePositions = robotX <$> scanlines
      Just y = findIndex isJust possiblePositions
      Just (x, dir) = possiblePositions !! y
  in Robot x y dir


turnLeft :: Direction -> Direction
turnLeft North = West
turnLeft West = South
turnLeft South = East
turnLeft East = North

turnRight :: Direction -> Direction
turnRight North = East
turnRight East = South
turnRight South = West
turnRight West = North

-- advanceLeft :: Map -> Robot -> Maybe (Move, Robot)
-- advanceLeft mp (Robot x y dir) =
--   let dir' = turnLeft dir

turnAndMove :: Map -> Robot -> (Direction -> Direction) -> Maybe (Int, Robot)
turnAndMove m r turn = if cellSpan^._1 > 1
                       then Just (cellSpan^._1 - 1, Robot (cellSpan^._2) (cellSpan^._3) dir')
                       else Nothing
  where dir' = turn (r^.dirL)

        go reachable coord@(x, y) ctr
          | getCell x y m = go coord (advance coord dir') (ctr + 1)
          | otherwise = (ctr, fst reachable, snd reachable)

        cellSpan = go (r^.xL, r^.yL) (r^.xL, r^.yL) 0



advance :: (Coord, Coord) -> Direction -> (Coord, Coord)
advance (x, y) North = (x, y - 1)
advance (x, y) South = (x, y + 1)
advance (x, y) East = (x + 1, y)
advance (x, y) West = (x - 1, y)

mapScaffold :: Map -> Robot -> [Move]
mapScaffold m r =
  case (turnAndMove m r turnLeft, turnAndMove m r turnRight) of
    (Just _, Just _) -> error $ "Both moves possible at " ++ show r
    (Just (dist, r'), _) -> L dist : mapScaffold m r'
    (_, Just (dist, r')) -> R dist : mapScaffold m r'
    _ -> []

part2 :: IO ()
part2 = do
  prog <- readProgram "17.txt"

  let mainRoutine = "B,C,B,A,C,B,A,C,B,A"
      rA = "L,6,L,10,L,10,L,6"
      rB = "L,6,L,4,R,12"
      rC = "L,6,R,12,R,12,L,8"
      wantFeed = "n"
      input = (concat $ intersperse "\n" [mainRoutine, rA, rB, rC, wantFeed]) ++ "\n"

  inputAction <- mkListInput (fromIntegral . ord <$> input)
  (outputRef, outputAction) <- mkListOutput

  let haltAction = liftIO $ putStrLn "Halted"
      opcodes = mkOpcodesTable (pureOpcodes ++ mkIoOpcodes haltAction inputAction outputAction)

  vm <- makeRunnable prog opcodes

  runIntcode vm $ do
    writeWord 0 2
    runVM

  out <- readIORef outputRef
  -- putStrLn $ chr . fromIntegral <$> reverse out
  print $ head out

main = part1

part1 :: IO ()
part1 = do
  prog <- readProgram "17.txt"

  inputAction <- mkListInput []
  (outputRef, outputAction) <- mkListOutput

  let haltAction = liftIO $ putStrLn "Halted"
      opcodes = mkOpcodesTable (pureOpcodes ++ mkIoOpcodes haltAction inputAction outputAction)

  vm <- makeRunnable prog opcodes

  runIntcode vm runVM
  unparsed <- reverse . fmap (chr.fromIntegral) <$> readIORef outputRef
  -- unparsed <- pure sampleMap2

  let parsed = parseMap unparsed
  putStrLn unparsed

  let robot = findDrone unparsed
  print robot
  -- print $ parsed robot turnRight
  print $ mapScaffold parsed robot


  -- print $ [sum $ uncurry (*) <$> getIntersections parsed]

  pure ()
