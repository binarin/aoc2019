{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.Extra (whileM)
import Data.IORef
import Data.Array.IO
import Data.Array.MArray
import Control.Monad (forM_, when, foldM, forM, void)
import Control.Concurrent.STM.TChan (TChan, newTChanIO, readTChan, writeTChan, unGetTChan, tryReadTChan, dupTChan)
import Control.Concurrent.STM (atomically)
import Control.Concurrent (forkIO)

program :: [Int]
program = [1,12,2,3,1,1,2,3,1,3,4, 3, 1, 5,0,3,2,1,6,19,1,9,19,23,2,23,10,27,1,27,5,31,1,31,6,35,1,6,35,39,2,39,13,43,1,9,43,47,2,9,47,51,1,51,6,55,2,55,10,59,1,59,5,63,2,10,63,67,2,9,67,71,1,71,5,75,2,10,75,79,1,79,6,83,2,10,83,87,1,5,87,91,2,9,91,95,1,95,5,99,1,99,2,103,1,103,13,0,99,2,14,0,0]
type Program = IOUArray Int Int

small :: [Int]
small = [1,9,10,3,
         2,3,11,0,
         99,
         30,40,50]

d5opcodes :: [Int]
d5opcodes = [
  4, 50,  7, 10, 09, 51,  4, 51, 99,  0,
  5,  5,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
 77,  0,  0,  0,  0,  0,  0,  0,  0,  0
 ]


d5_2_opcodes :: [Int]
d5_2_opcodes = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31, 1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104, 999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]

d5 :: [Int]
d5 = [3,225,1,225,6,6,1100,1,238,225,104,0,1101,40,27,224,101,-67,224,224,4,224,1002,223,8,223,1001,224,2,224,1,224,223,223,1101,33,38,225,1102,84,60,225,1101,65,62,225,1002,36,13,224,1001,224,-494,224,4,224,1002,223,8,223,1001,224,3,224,1,223,224,223,1102,86,5,224,101,-430,224,224,4,224,1002,223,8,223,101,6,224,224,1,223,224,223,1102,23,50,225,1001,44,10,224,101,-72,224,224,4,224,102,8,223,223,101,1,224,224,1,224,223,223,102,47,217,224,1001,224,-2303,224,4,224,102,8,223,223,101,2,224,224,1,223,224,223,1102,71,84,225,101,91,40,224,1001,224,-151,224,4,224,1002,223,8,223,1001,224,5,224,1,223,224,223,1101,87,91,225,1102,71,19,225,1,92,140,224,101,-134,224,224,4,224,1002,223,8,223,101,1,224,224,1,224,223,223,2,170,165,224,1001,224,-1653,224,4,224,1002,223,8,223,101,5,224,224,1,223,224,223,1101,49,32,225,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,1107,226,677,224,1002,223,2,223,1006,224,329,101,1,223,223,8,226,226,224,1002,223,2,223,1005,224,344,101,1,223,223,1007,677,226,224,102,2,223,223,1005,224,359,101,1,223,223,8,226,677,224,102,2,223,223,1005,224,374,101,1,223,223,1107,677,677,224,1002,223,2,223,1005,224,389,1001,223,1,223,108,226,677,224,102,2,223,223,1005,224,404,1001,223,1,223,108,677,677,224,1002,223,2,223,1006,224,419,101,1,223,223,107,677,677,224,102,2,223,223,1006,224,434,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,449,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,464,101,1,223,223,1108,226,677,224,1002,223,2,223,1006,224,479,1001,223,1,223,1108,677,677,224,1002,223,2,223,1005,224,494,101,1,223,223,7,677,677,224,1002,223,2,223,1005,224,509,101,1,223,223,1007,677,677,224,1002,223,2,223,1005,224,524,101,1,223,223,7,677,226,224,1002,223,2,223,1005,224,539,101,1,223,223,1107,677,226,224,102,2,223,223,1006,224,554,101,1,223,223,107,226,677,224,1002,223,2,223,1005,224,569,101,1,223,223,107,226,226,224,1002,223,2,223,1005,224,584,101,1,223,223,1108,677,226,224,102,2,223,223,1006,224,599,1001,223,1,223,1008,677,677,224,102,2,223,223,1006,224,614,101,1,223,223,7,226,677,224,102,2,223,223,1005,224,629,101,1,223,223,1008,226,677,224,1002,223,2,223,1006,224,644,101,1,223,223,1007,226,226,224,1002,223,2,223,1005,224,659,1001,223,1,223,1008,226,226,224,102,2,223,223,1006,224,674,1001,223,1,223,4,223,99,226]

d7 :: [Int]
d7 = [3,8,1001,8,10,8,105,1,0,0,21,42,55,64,77,94,175,256,337,418,99999,3,9,102,4,9,9,1001,9,5,9,102,2,9,9,101,3,9,9,4,9,99,3,9,102,2,9,9,101,5,9,9,4,9,99,3,9,1002,9,4,9,4,9,99,3,9,102,4,9,9,101,5,9,9,4,9,99,3,9,102,5,9,9,1001,9,3,9,1002,9,5,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1001,9,2,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,1001,9,1,9,4,9,99]

makeRunnable :: [Int] -> IO Program
makeRunnable program = newListArray (0, length program - 1) program

readArg :: Program -> Int -> Int -> IO Int
readArg prog ip argNo = do
  opcode <- readArray prog ip
  arg <- readArray prog (ip + argNo)
  let argMode = (opcode `div` (10^(argNo+1))) `rem` 10
  case argMode of
    0 -> readArray prog arg
    1 -> pure arg

writeArg :: Program -> Int -> Int -> Int -> IO ()
writeArg prog ip argNo value = do
  addr <- readArray prog (ip + argNo)
  writeArray prog addr value

binaryOp :: Program -> Int -> (Int -> Int -> Int) -> IO ()
binaryOp prog ip f = do
  result <- f <$> readArg prog ip 1 <*> readArg prog ip 2
  writeArg prog ip 3 result

jumpIfOp :: Program -> Int -> (Int -> Bool) -> IO Int
jumpIfOp prog ip shouldJump = do
  shouldJump <$> readArg prog ip 1 >>= \case
    False -> pure $ ip + 3
    True -> readArg prog ip 2

boolToValue :: Bool -> Int
boolToValue True = 1
boolToValue False = 0

cmpOp :: Program -> Int -> (Int -> Int -> Bool) -> IO ()
cmpOp prog ip cmp = do
  value <- cmp <$> readArg prog ip 1 <*> readArg prog ip 2
  writeArg prog ip 3 (boolToValue value)

inputOp :: Program -> Int -> TChan Int -> IO ()
inputOp prog ip chan = do
  atomically (readTChan chan) >>= writeArg prog ip 1

outputOp :: Program -> Int -> TChan Int -> IO ()
outputOp prog ip chan = do
  val <- readArg prog ip 1
  atomically $ writeTChan chan val

run :: Program -> TChan Int -> TChan Int -> TChan () -> IO ()
run prog input output halt = void $ forkIO $ go 0
  where
    go :: Int -> IO ()
    go ip = do
      (`mod` 100) <$> readArray prog ip >>= \case
        1 -> binaryOp prog ip (+) >> go (ip + 4)
        2 -> binaryOp prog ip (*) >> go (ip + 4)
        3 -> inputOp prog ip input >> go (ip + 2)
        4 -> outputOp prog ip output >> go (ip + 2)
        5 -> jumpIfOp prog ip (/= 0) >>= go
        6 -> jumpIfOp prog ip (== 0) >>= go
        7 -> cmpOp prog ip (<) >> go (ip + 4)
        8 -> cmpOp prog ip (==) >> go (ip + 4)
        99 -> void $ atomically $ writeTChan halt ()
        bad -> error $ "bad op " ++ show bad

d7_2_small_1 :: [Int]
d7_2_small_1 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26, 27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]

d7_2_small_2 :: [Int]
d7_2_small_2 = [3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54, -5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4, 53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10]

main :: IO ()
main = do
  runFeedbackLoop d7_2_small_1 [9,8,7,6,5] >>= putStrLn . show
  runFeedbackLoop d7_2_small_2 [9,7,8,5,6] >>= putStrLn . show
  signals <- forM (permutations [5..9]) $ \phases -> do
    runFeedbackLoop d7 phases
  putStrLn $ show $ maximum signals
  pure ()

runFeedbackLoop :: [Int] -> [Int] -> IO Int
runFeedbackLoop prog phases = do
  input <- newTChanIO
  loopOutput <- atomically $ dupTChan input

  curInp <- newIORef input

  forM (init phases) $ \phase -> do
    input <- readIORef curInp
    atomically $ unGetTChan input phase
    output <- newTChanIO
    halt <- newTChanIO
    p <- makeRunnable prog
    run p input output halt
    writeIORef curInp output

  lastP <- makeRunnable prog

  preLastOut <- readIORef curInp
  atomically $ unGetTChan preLastOut (last phases)

  halt <- newTChanIO
  run lastP preLastOut input halt

  atomically $ writeTChan input 0
  void $ atomically $ readTChan halt

  lastOut <- newIORef 0
  whileM $ do
    atomically (tryReadTChan loopOutput) >>= \case
      Nothing -> pure False
      Just val -> do
        writeIORef lastOut val
        pure True

  readIORef lastOut

permutations :: [a] -> [[a]]
permutations [] = []
permutations [x] = [[x]]
permutations vals =
  let subs = subproblems vals
      expandSub (x, rest) = (x:) <$> permutations rest
  in mconcat $ expandSub <$> subs

subproblems :: [a] -> [(a, [a])]
subproblems = go []
  where
    go skipped [] = []
    go skipped (x:xs) = (x, reverse skipped ++ xs) : go (x:skipped) xs

test :: IO ()
test = do
  pure ()
