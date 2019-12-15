{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Monad.IO.Class
import Control.Lens.TH
import System.Exit (exitSuccess)
import Control.Concurrent.MVar
import Data.List.Extra (chunksOf)
import Data.ByteString (ByteString)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import qualified Data.HashTable.IO as H
import Control.Lens hiding (Empty)
import Control.Monad.Extra (whileM)
import Data.IORef
import Data.Array.MArray
import Data.Array (Array, (!))
import Control.Loop

import Control.Monad (forM_, when, foldM, forM, void, zipWithM, join)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan, unGetTChan, tryReadTChan, dupTChan, isEmptyTChan)
import Control.Concurrent.STM (atomically)
import Control.Concurrent (forkIO)

import IntCode

data Tile = Empty | Wall | Block | Padddle | Ball deriving (Eq, Show, Enum)

data Arcade = Arcade { _arcadeInputL :: TChan MachineWord
                     , _arcadeScreenRefL :: MVar (H.LinearHashTable (Int, Int) Tile)
                     , _arcadeScoreRefL :: MVar MachineWord
                     }

makeFields ''Arcade


dump :: Show a => a -> App ()
dump a = liftIO $ putStrLn $ show a

data Direction = North | East | South | West
type Hull = H.LinearHashTable (Int, Int) Bool

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

moveInDirection :: Direction -> (Int, Int) -> (Int, Int)
moveInDirection North = _2 -~ 1
moveInDirection South = _2 +~ 1
moveInDirection West = _1 -~ 1
moveInDirection East = _1 +~ 1

runDay11 :: IO ()
runDay11 = do
  hull :: Hull <- H.new
  positionRef :: IORef (Int, Int) <- newIORef (0, 0)
  dirRef :: IORef Direction <- newIORef North
  inputBuffer :: IORef [MachineWord] <- newIORef []

  let doTurn 0 = modifyIORef dirRef turnLeft
      doTurn 1 = modifyIORef dirRef turnRight

      doMove = do
        dir <- readIORef dirRef
        modifyIORef positionRef (moveInDirection dir)

      colorToBool 0 = False
      colorToBool 1 = True

      paintCurrent isWhite = do
        coord <- readIORef positionRef
        H.mutate hull coord $ \case
           Nothing -> if isWhite
                      then (Just True, ())
                      else (Nothing, ())
           Just _ -> (Just isWhite, ())

      haltAction = putStrLn "halted"

      inputAction = do
        (x, y) <- readIORef positionRef
        H.lookup hull (x, y) >>= \case
          Nothing -> pure 0
          Just False -> pure 0
          Just True -> pure 1

      outputAction value = do
        readIORef inputBuffer >>= \case
          (color:_) -> do
            paintCurrent (colorToBool color)
            doTurn value
            doMove
            writeIORef inputBuffer []
          [] -> do
            writeIORef inputBuffer [value]

      opcodes = mkOpcodesTable $ pureOpcodes ++ mkIoOpcodes haltAction inputAction outputAction

      scratch = [104, 1, 99, 50,  4, 50, 99, 51, 99,  0,
                  5,  5,  0,  0,  0,  0,  0,  0,  0,  0,
                  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
                  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
                  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
                 42, 13,  0,  0,  0,  0,  0,  0,  0,  0
                ]
      part1 = do
        prog <- makeRunnable d11 opcodes
        runIntcode prog runVM
        result <- H.toList hull
        putStrLn $ show $ length result

      part2 = do
        prog <- makeRunnable d11 opcodes
        H.insert hull (0,0) True
        runIntcode prog runVM
        allCells <- H.toList hull
        let cells = view _1 <$> filter (view _2) allCells
            panel (x, y) = Color white (Translate (fromIntegral x) (fromIntegral $ - y) square)
            allPanels = Pictures $ panel <$> cells

        display (InWindow "Nice Window" (200, 200) (10, 10)) black allPanels
        pure ()

  part2
  pure ()

main :: IO ()
main = pure ()

blockSize :: Int
blockSize = 8

tileAt :: Int -> Int -> Picture -> Picture
tileAt x y pic = Translate (fromIntegral $ x * blockSize) (negate $ fromIntegral $ y * blockSize) $ Scale (fromIntegral blockSize) (fromIntegral blockSize) pic

square :: Picture
square = Polygon [(0, 0), (0, 1), (1, 1), (1, 0)]

tileToPicture :: Tile -> Picture
tileToPicture Empty = Blank
tileToPicture Wall = Color red square
tileToPicture Block = Color (makeColorI 127 127 127 255) square
tileToPicture Padddle = Color blue square
tileToPicture Ball = Color orange $ Translate 0.5 0.5 $ Circle 0.5


drawArcade :: [MachineWord] -> Picture
drawArcade output = Pictures $ go (reverse output)
  where
    go :: [MachineWord] -> [Picture]
    go (x:y:tile:rest) = tileAt (fromIntegral x) (fromIntegral y) (tileToPicture $ toEnum $ fromIntegral tile) : go rest
    go _ = []

mkArcade :: IO (Arcade, InputAction, OutputAction, HaltAction)
mkArcade = do
  inputChan <- newTChanIO
  let inputAction = do
        atomically $ readTChan inputChan

  screenHash :: H.LinearHashTable (Int, Int) Tile <- H.new
  screen <- newMVar screenHash
  buffer <- newIORef []
  scoreRef <- newMVar 0

  let replaceTile Empty _ = (Nothing, ())
      replaceTile new _ = (Just new, ())

      outputAction :: MachineWord -> IO ()
      outputAction word = do
        readIORef buffer >>= \case
          (y:x:_)
            | x < 0 -> do
                modifyMVar_ scoreRef (const $ pure word)
                writeIORef buffer []
            | otherwise -> do
                modifyMVar_ screen $ \hash -> do
                  H.mutate hash (fromIntegral x, fromIntegral y) (replaceTile $ toEnum $ fromIntegral word)
                  pure hash
                writeIORef buffer []
          incomplete -> do
            modifyIORef buffer (word:)

  let arcade = Arcade inputChan screen scoreRef

  let haltAction = do
        readMVar scoreRef >>= print
        exitSuccess

  pure $ (arcade, inputAction, outputAction, haltAction)

arcadeToPicture :: Arcade -> IO Picture
arcadeToPicture arcade = do
  cells <- modifyMVar (arcade^.screenRefL) $ \hash -> do
    l <- H.toList hash
    pure (hash ,l)
  let go [] = [Blank]
      go (((x, y), tile):rest) = tileAt x y (tileToPicture tile) : go rest
  pure $ Pictures $ go cells

runDay13Arcade :: IO ()
runDay13Arcade = do
  (arcade, inputAction, outputAction, haltAction) <- mkArcade

  d13 <- readProgram "13.txt"

  let opcodes = mkOpcodesTable (pureOpcodes ++ mkIoOpcodes haltAction inputAction outputAction)
  p <- makeRunnable d13 opcodes
  forkIO $ runIntcode p $ do
    writeWord 0 2
    runVM

  let doStep time arcade = do
        atomically (isEmptyTChan $ arcade^.inputL) >>= \case
          True -> do
            atomically $ writeTChan (arcade^.inputL) 0

          False -> pure ()
        pure arcade

  playIO
    (InWindow "Nice Window" (200, 200) (10, 10))
    black
    60
    arcade
    arcadeToPicture
    (\event arcade -> pure arcade)
    doStep

  pure ()

runDay13 :: IO ()
runDay13 = do
  inputAction <- mkListInput []
  (outputRef, outputAction) <- mkListOutput
  let haltAction = putStrLn "halted"
      opcodes = mkOpcodesTable (pureOpcodes ++ mkIoOpcodes haltAction inputAction outputAction)
  d13 <- readProgram "13.txt"
  p <- makeRunnable d13 opcodes
  runIntcode p runVM
  output <- readIORef outputRef
  let chunked :: [[MachineWord]] = chunksOf 3 $ reverse output

  visibleSet :: H.LinearHashTable (Int, Int) Tile <- H.new
  forM_ chunked $ \(x:y:b:_) -> do
    H.insert visibleSet (fromIntegral x, fromIntegral y) (toEnum $ fromIntegral b)

  visible <- H.toList visibleSet
  let blocks = [ True | (_, Block) <- visible ]
  print $ length blocks

  display (InWindow "Nice Window" (200, 200) (10, 10)) black (drawArcade output)
  pure ()

runDay8 :: IO ()
runDay8 = do
  let selfCopy = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]

  inputAction <- mkListInput [2]
  (outputRef, outputAction) <- mkListOutput
  let haltAction = putStrLn "halted"
      ioOpcodes = mkIoOpcodes haltAction inputAction outputAction
      opcodes = pureOpcodes ++ ioOpcodes

  p <- makeRunnable selfCopy (mkOpcodesTable opcodes)
  let comp :: App () = do
        runVM
        readWord 50 >>= dump
  runIntcode p runVM
  readIORef outputRef >>= putStrLn . show . reverse
  pure ()

d8 :: [MachineWord]
d8 = [1102,34463338,34463338,63,1007,63,34463338,63,1005,63,53,1101,0,3,1000,109,988,209,12,9,1000,209,6,209,3,203,0,1008,1000,1,63,1005,63,65,1008,1000,2,63,1005,63,904,1008,1000,0,63,1005,63,58,4,25,104,0,99,4,0,104,0,99,4,17,104,0,99,0,0,1102,1,432,1027,1101,439,0,1026,1101,0,36,1010,1101,0,34,1018,1102,278,1,1029,1101,0,24,1002,1102,1,20,1016,1102,1,31,1011,1102,319,1,1024,1102,21,1,1012,1102,1,763,1022,1102,1,25,1007,1101,0,287,1028,1102,32,1,1008,1101,0,22,1013,1102,38,1,1001,1101,0,314,1025,1102,35,1,1009,1102,1,23,1015,1102,39,1,1019,1102,27,1,1000,1102,1,37,1003,1102,1,28,1017,1101,0,0,1020,1101,0,29,1004,1102,1,30,1006,1102,1,756,1023,1102,1,33,1005,1101,0,1,1021,1102,26,1,1014,109,13,2108,28,-7,63,1005,63,201,1001,64,1,64,1105,1,203,4,187,1002,64,2,64,109,8,21107,40,41,-3,1005,1018,225,4,209,1001,64,1,64,1105,1,225,1002,64,2,64,109,-3,1206,2,239,4,231,1105,1,243,1001,64,1,64,1002,64,2,64,109,-21,1201,6,0,63,1008,63,35,63,1005,63,267,1001,64,1,64,1105,1,269,4,249,1002,64,2,64,109,35,2106,0,-4,4,275,1001,64,1,64,1105,1,287,1002,64,2,64,109,-11,1205,-1,303,1001,64,1,64,1105,1,305,4,293,1002,64,2,64,109,8,2105,1,-5,4,311,1106,0,323,1001,64,1,64,1002,64,2,64,109,-7,21108,41,38,-6,1005,1016,339,1106,0,345,4,329,1001,64,1,64,1002,64,2,64,109,2,21102,42,1,-8,1008,1016,45,63,1005,63,369,1001,64,1,64,1105,1,371,4,351,1002,64,2,64,109,-14,21101,43,0,1,1008,1011,43,63,1005,63,397,4,377,1001,64,1,64,1106,0,397,1002,64,2,64,109,-8,21101,44,0,8,1008,1010,47,63,1005,63,417,1105,1,423,4,403,1001,64,1,64,1002,64,2,64,109,25,2106,0,0,1001,64,1,64,1105,1,441,4,429,1002,64,2,64,109,-20,2107,37,-6,63,1005,63,463,4,447,1001,64,1,64,1106,0,463,1002,64,2,64,109,8,2108,25,-8,63,1005,63,485,4,469,1001,64,1,64,1106,0,485,1002,64,2,64,109,-1,21107,45,44,-1,1005,1013,505,1001,64,1,64,1106,0,507,4,491,1002,64,2,64,109,-11,1207,-1,25,63,1005,63,529,4,513,1001,64,1,64,1106,0,529,1002,64,2,64,109,23,1206,-5,545,1001,64,1,64,1106,0,547,4,535,1002,64,2,64,109,-31,2102,1,5,63,1008,63,27,63,1005,63,569,4,553,1106,0,573,1001,64,1,64,1002,64,2,64,109,27,21102,46,1,-9,1008,1013,46,63,1005,63,595,4,579,1105,1,599,1001,64,1,64,1002,64,2,64,109,-26,2101,0,6,63,1008,63,24,63,1005,63,625,4,605,1001,64,1,64,1106,0,625,1002,64,2,64,109,5,1208,0,37,63,1005,63,645,1001,64,1,64,1105,1,647,4,631,1002,64,2,64,109,7,2102,1,-3,63,1008,63,31,63,1005,63,671,1001,64,1,64,1105,1,673,4,653,1002,64,2,64,109,2,1202,-5,1,63,1008,63,33,63,1005,63,699,4,679,1001,64,1,64,1105,1,699,1002,64,2,64,109,-4,2101,0,-3,63,1008,63,35,63,1005,63,719,1105,1,725,4,705,1001,64,1,64,1002,64,2,64,109,-5,1207,4,32,63,1005,63,741,1106,0,747,4,731,1001,64,1,64,1002,64,2,64,109,29,2105,1,-7,1001,64,1,64,1106,0,765,4,753,1002,64,2,64,109,-26,2107,36,5,63,1005,63,781,1105,1,787,4,771,1001,64,1,64,1002,64,2,64,109,10,1201,-6,0,63,1008,63,32,63,1005,63,809,4,793,1106,0,813,1001,64,1,64,1002,64,2,64,109,3,21108,47,47,-5,1005,1012,835,4,819,1001,64,1,64,1106,0,835,1002,64,2,64,109,-24,1202,9,1,63,1008,63,25,63,1005,63,859,1001,64,1,64,1106,0,861,4,841,1002,64,2,64,109,19,1205,9,875,4,867,1106,0,879,1001,64,1,64,1002,64,2,64,109,-3,1208,-1,32,63,1005,63,897,4,885,1106,0,901,1001,64,1,64,4,64,99,21102,27,1,1,21101,915,0,0,1105,1,922,21201,1,60043,1,204,1,99,109,3,1207,-2,3,63,1005,63,964,21201,-2,-1,1,21102,1,942,0,1106,0,922,21202,1,1,-1,21201,-2,-3,1,21101,957,0,0,1106,0,922,22201,1,-1,-2,1105,1,968,22102,1,-2,-2,109,-3,2105,1,0]

d11 :: [MachineWord]
d11 = [3,8,1005,8,330,1106,0,11,0,0,0,104,1,104,0,3,8,102,-1,8,10,101,1,10,10,4,10,1008,8,0,10,4,10,102,1,8,29,2,9,4,10,1006,0,10,1,1103,17,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,101,0,8,61,1006,0,21,1006,0,51,3,8,1002,8,-1,10,101,1,10,10,4,10,108,1,8,10,4,10,1001,8,0,89,1,102,19,10,1,1107,17,10,1006,0,18,3,8,1002,8,-1,10,1001,10,1,10,4,10,1008,8,1,10,4,10,1001,8,0,123,1,9,2,10,2,1105,10,10,2,103,9,10,2,1105,15,10,3,8,102,-1,8,10,1001,10,1,10,4,10,1008,8,0,10,4,10,102,1,8,161,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,182,3,8,1002,8,-1,10,101,1,10,10,4,10,1008,8,0,10,4,10,101,0,8,205,2,1102,6,10,1006,0,38,2,1007,20,10,2,1105,17,10,3,8,102,-1,8,10,1001,10,1,10,4,10,108,1,8,10,4,10,1001,8,0,241,3,8,102,-1,8,10,101,1,10,10,4,10,108,1,8,10,4,10,101,0,8,263,1006,0,93,2,5,2,10,2,6,7,10,3,8,102,-1,8,10,101,1,10,10,4,10,108,0,8,10,4,10,1001,8,0,296,1006,0,81,1006,0,68,1006,0,76,2,4,4,10,101,1,9,9,1007,9,1010,10,1005,10,15,99,109,652,104,0,104,1,21102,825594262284,1,1,21102,347,1,0,1105,1,451,21101,0,932855939852,1,21101,358,0,0,1106,0,451,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,3,10,104,0,104,1,3,10,104,0,104,0,3,10,104,0,104,1,21102,1,235152649255,1,21101,405,0,0,1105,1,451,21102,235350879235,1,1,21102,416,1,0,1106,0,451,3,10,104,0,104,0,3,10,104,0,104,0,21102,988757512972,1,1,21101,439,0,0,1106,0,451,21102,1,988669698828,1,21101,0,450,0,1106,0,451,99,109,2,22101,0,-1,1,21102,40,1,2,21102,1,482,3,21102,472,1,0,1106,0,515,109,-2,2105,1,0,0,1,0,0,1,109,2,3,10,204,-1,1001,477,478,493,4,0,1001,477,1,477,108,4,477,10,1006,10,509,1101,0,0,477,109,-2,2106,0,0,0,109,4,1202,-1,1,514,1207,-3,0,10,1006,10,532,21102,1,0,-3,21202,-3,1,1,21202,-2,1,2,21102,1,1,3,21102,1,551,0,1106,0,556,109,-4,2105,1,0,109,5,1207,-3,1,10,1006,10,579,2207,-4,-2,10,1006,10,579,22101,0,-4,-4,1105,1,647,21201,-4,0,1,21201,-3,-1,2,21202,-2,2,3,21102,598,1,0,1105,1,556,21202,1,1,-4,21101,0,1,-1,2207,-4,-2,10,1006,10,617,21102,1,0,-1,22202,-2,-1,-2,2107,0,-3,10,1006,10,639,21202,-1,1,1,21102,1,639,0,105,1,514,21202,-2,-1,-2,22201,-4,-2,-4,109,-5,2105,1,0]


-- program :: [Integer]
-- program = [1,12,2,3,1,1,2,3,1,3,4, 3, 1, 5,0,3,2,1,6,19,1,9,19,23,2,23,10,27,1,27,5,31,1,31,6,35,1,6,35,39,2,39,13,43,1,9,43,47,2,9,47,51,1,51,6,55,2,55,10,59,1,59,5,63,2,10,63,67,2,9,67,71,1,71,5,75,2,10,75,79,1,79,6,83,2,10,83,87,1,5,87,91,2,9,91,95,1,95,5,99,1,99,2,103,1,103,13,0,99,2,14,0,0]

-- small :: [Int]
-- small = [1,9,10,3,
--          2,3,11,0,
--          99,
--          30,40,50]

-- d5opcodes :: [Int]
-- d5opcodes = [
--   4, 50,  7, 10, 09, 51,  4, 51, 99,  0,
--   5,  5,  0,  0,  0,  0,  0,  0,  0,  0,
--   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
--   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
--   0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
--  77,  0,  0,  0,  0,  0,  0,  0,  0,  0
--  ]


-- d5_2_opcodes :: [Int]
-- d5_2_opcodes = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31, 1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104, 999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]

-- d5 :: [Int]
-- d5 = [3,225,1,225,6,6,1100,1,238,225,104,0,1101,40,27,224,101,-67,224,224,4,224,1002,223,8,223,1001,224,2,224,1,224,223,223,1101,33,38,225,1102,84,60,225,1101,65,62,225,1002,36,13,224,1001,224,-494,224,4,224,1002,223,8,223,1001,224,3,224,1,223,224,223,1102,86,5,224,101,-430,224,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,1102,23,50,225,1001,44,10,224,101,-72,224,224,4,224,102,8,223,223,101,1,224,224,1,224,223,223,102,47,217,224,1001,224,-2303,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1102,71,84,225,101,91,40,224,1001,224,-151,224,4,224,1002,223,8,223,1001,224,5,224,1,223,224,223,1101,87,91,225,1102,71,19,225,1,92,140,224,101,-134,224,224,4,224,1002,223,8,223,101,1,224,224,1,224,223,223,2,170,165,224,1001,224,-1653,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,1101,49,32,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1107,226,677,224,1002,223,2,223,1006,224,329,101,1,223,223,8,226,226,224,1002,223,2,223,1005,224,344,101,1,223,223,1007,677,226,224,102,2,223,223,1005,224,359,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,374,101,1,223,223,1107,677,677,224,1002,223,2,223,1005,224,389,1001,223,1,223,108,226,677,224,102,2,223,223,1005,224,404,1001,223,1,223,108,677,677,224,1002,223,2,223,1006,224,419,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,434,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,449,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,464,101,1,223,223,1108,226,677,224,1002,223,2,223,1006,224,479,1001,223,1,223,1108,677,677,224,1002,223,2,223,1005,224,494,101,1,223,223,7,677,677,224,1002,223,2,223,1005,224,509,101,1,223,223,1007,677,677,224,1002,223,2,223,1005,224,524,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,539,101,1,223,223,1107,677,226,224,102,2,223,223,1006,224,554,101,1,223,223,107,226,677,224,1002,223,2,223,1005,224,569,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,584,101,1,223,223,1108,677,226,224,102,2,223,223,1006,224,599,1001,223,1,223,1008,677,677,224,102,2,223,223,1006,224,614,101,1,223,223,7,226,677,224,102,2,223,223,1005,224,629,101,1,223,223,1008,226,677,224,1002,223,2,223,1006,224,644,101,1,223,223,1007,226,226,224,1002,223,2,223,1005,224,659,1001,223,1,223,1008,226,226,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226]

-- d7 :: [Int]
-- d7 = [3,8,1001,8,10,8,105,1,0,0,21,42,55,64,77,94,175,256,337,418,99999,3,9,102,4,9,9,1001,9,5,9,102,2,9,9,101,3,9,9,4,9,99,3,9,102,2,9,9,101,5,9,9,4,9,99,3,9,1002,9,4,9,4,9,99,3,9,102,4,9,9,101,5,9,9,4,9,99,3,9,102,5,9,9,1001,9,3,9,1002,9,5,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99]


-- d7_2_small_1 :: [Int]
-- d7_2_small_1 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26, 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

-- d7_2_small_2 :: [Int]
-- d7_2_small_2 = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54, -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4, 53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]

-- main :: IO ()
-- main = do
--   runFeedbackLoop d7_2_small_1 [9,8,7,6,5] >>= putStrLn . show
--   runFeedbackLoop d7_2_small_2 [9,7,8,5,6] >>= putStrLn . show
--   signals <- forM (permutations [5..9]) $ \phases -> do
--     runFeedbackLoop d7 phases
--   putStrLn $ show $ maximum signals
--   pure ()

-- runFeedbackLoop :: [Int] -> [Int] -> IO Int
-- runFeedbackLoop prog phases = do
--   input <- newTChanIO
--   loopOutput <- atomically $ dupTChan input

--   curInp <- newIORef input

--   forM (init phases) $ \phase -> do
--     input <- readIORef curInp
--     atomically $ unGetTChan input phase
--     output <- newTChanIO
--     halt <- newTChanIO
--     p <- makeRunnable prog
--     run p input output halt
--     writeIORef curInp output

--   lastP <- makeRunnable prog

--   preLastOut <- readIORef curInp
--   atomically $ unGetTChan preLastOut (last phases)

--   halt <- newTChanIO
--   run lastP preLastOut input halt

--   atomically $ writeTChan input 0
--   void $ atomically $ readTChan halt

--   lastOut <- newIORef 0
--   whileM $ do
--     atomically (tryReadTChan loopOutput) >>= \case
--       Nothing -> pure False
--       Just val -> do
--         writeIORef lastOut val
--         pure True

--   readIORef lastOut

-- permutations :: [a] -> [[a]]
-- permutations [] = []
-- permutations [x] = [[x]]
-- permutations vals =
--   let subs = subproblems vals
--       expandSub (x, rest) = (x:) <$> permutations rest
--   in mconcat $ expandSub <$> subs

-- subproblems :: [a] -> [(a, [a])]
-- subproblems = go []
--   where
--     go skipped [] = []
--     go skipped (x:xs) = (x, reverse skipped ++ xs) : go (x:skipped) xs

-- test :: IO ()
-- test = do
--   pure ()

