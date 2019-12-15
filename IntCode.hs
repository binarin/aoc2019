{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module IntCode
  ( MachineWord
  , MachinePointer
  , ArgumentNo
  , OpcodeTable
  , Program(..)
  , App
  , InputAction
  , HaltAction
  , OutputAction
  , memoryL
  , ipL
  , relativeBaseL
  , haltedL
  , opcodesL
  , runIntcode
  , readProgram
  , writeWord
  , readWordRelativeIp
  , readWord
  , readWordRelativeBase
  , makeRunnable
  , runVM
  , mkOpcodesTable
  , pureOpcodes
  , mkIoOpcodes
  , mkListInput
  , mkListOutput
  ) where

import Data.IORef
import Control.Lens
import Control.Lens.TH
import Data.Array (Array, (!))
import Data.Array.IO
import Control.Monad.State.Strict
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Control.Exception (throwIO, Exception)
import Data.Array.ST (STArray, runSTArray)


type MachineWord = Integer
type MachinePointer = Int
type ArgumentNo = Int

type OpcodeTable = Array Int (App ())

data Program = Program { _programMemoryL :: IOArray MachinePointer MachineWord
                       , _programIpL :: MachinePointer
                       , _programRelativeBaseL :: MachinePointer
                       , _programHaltedL :: Bool
                       , _programOpcodesL :: OpcodeTable
                       }
newtype App a = App { runApp :: StateT Program IO a } deriving (Functor, Applicative, Monad, MonadIO, MonadState Program)

type HaltAction = IO ()
type InputAction = IO MachineWord
type OutputAction = MachineWord -> IO ()


makeFields ''Program


readProgram :: FilePath -> IO [MachineWord]
readProgram path = do
  content <- B.readFile path
  pure $ read . C8.unpack <$> C8.split ',' content


runIntcode :: Program -> App a -> IO ()
runIntcode p action = void $ execStateT (runApp action) p


runVM :: App ()
runVM = go
  where
    go :: App ()
    go = do
      opcode <- (`mod` 100) <$> readWordRelativeIp 0
      action <- uses opcodesL (! fromIntegral opcode)
      action
      use haltedL >>= \case
        True -> pure ()
        False -> go

makeRunnable :: [MachineWord] -> OpcodeTable -> IO Program
makeRunnable rawProgram opcodes = do
  let initialLength = fromIntegral (length rawProgram) - 1
  memory <- newListArray (0, initialLength) rawProgram
  let ip = 0
      relativeBase = 0
      halted = False
  pure $ Program memory ip relativeBase halted opcodes

ensureMemoryBigEnough :: MachinePointer -> App ()
ensureMemoryBigEnough ptr = do
  memory <- use memoryL
  (_, upper) <- liftIO $ getBounds memory

  when (ptr > upper) $ do
    let upper' = max (upper * 2) ptr
    memory' <- liftIO $ newArray (0, upper') 0
    let copyAtPosition i = readArray memory i >>= writeArray memory' i
    liftIO $ forM_ [0..upper] copyAtPosition
    memoryL .= memory'

readWord :: MachinePointer -> App MachineWord
readWord addr = do
  ensureMemoryBigEnough addr
  use memoryL >>= (liftIO . flip readArray addr)

readWordRelativeIp :: Int -> App MachineWord
readWordRelativeIp offset = readWord =<< uses ipL (+offset)

readWordRelativeBase :: Int -> App MachineWord
readWordRelativeBase offset = readWord =<< uses relativeBaseL (+offset)

writeWord :: MachinePointer -> MachineWord -> App ()
writeWord addr value = do
  ensureMemoryBigEnough addr
  m <- use memoryL
  liftIO $ writeArray m addr value

infixl 4 <♭>
(<♭>) :: Monad f => f (a -> f b) -> f a -> f b
f <♭> a = join $ f <*> a

writeWordRelativeBase :: Int -> MachineWord -> App ()
writeWordRelativeBase offset value =
  writeWord
    <$> uses relativeBaseL (+ fromIntegral offset)
    <♭> pure value

argMode :: MachineWord -> ArgumentNo -> Int
argMode opcode argNo = fromIntegral $ (opcode `div` (10^(argNo+1))) `rem` 10

readArg :: ArgumentNo -> App MachineWord
readArg argNo = do
  opcode <- readWordRelativeIp 0
  arg <- readWordRelativeIp argNo
  case argMode opcode argNo of
    0 -> readWord (fromIntegral arg)
    1 -> pure arg
    2 -> readWordRelativeBase (fromIntegral arg)

writeArg :: ArgumentNo -> MachineWord -> App ()
writeArg argNo value = do
  opcode <- readWordRelativeIp 0
  arg <- readWordRelativeIp argNo
  case argMode opcode argNo of
    0 -> writeWord (fromIntegral arg) value
    2 -> writeWordRelativeBase (fromIntegral arg) value

binaryOp :: (MachineWord -> MachineWord -> MachineWord) -> App ()
binaryOp f = do
  f <$> readArg 1 <*> readArg 2 >>= writeArg 3
  ipL += 4

inputOp :: InputAction -> App ()
inputOp action = do
  value <- liftIO $ action
  writeArg 1 value
  ipL += 2

relativeBaseAddOp :: App ()
relativeBaseAddOp = do
  delta <- readArg 1
  relativeBaseL += fromIntegral delta
  ipL += 2

pureOpcodes :: [(Int, App ())]
pureOpcodes = [(1, binaryOp (+))
              ,(2, binaryOp (*))
              ,(5, jumpIfOp (/= 0))
              ,(6, jumpIfOp (== 0))
              ,(7, cmpOp (<))
              ,(8, cmpOp (==))
              ,(9, relativeBaseAddOp)
              ]

haltOp :: HaltAction -> App ()
haltOp halt = do
  haltedL .= True
  liftIO $ halt

outputOp :: OutputAction -> App ()
outputOp action = do
  readArg 1 >>= liftIO . action
  ipL += 2

jumpIfOp :: (MachineWord -> Bool) -> App ()
jumpIfOp shouldJump = do
  shouldJump <$> readArg 1 >>= \case
    False -> ipL += 3
    True -> do
      ip' <- readArg 2
      ipL .= fromIntegral ip'

boolToValue :: Bool -> MachineWord
boolToValue True = 1
boolToValue False = 0

cmpOp :: (MachineWord -> MachineWord -> Bool) -> App ()
cmpOp cmp = do
  value <- cmp <$> readArg 1 <*> readArg 2
  writeArg 3 (boolToValue value)
  ipL += 4

mkIoOpcodes :: HaltAction -> InputAction -> OutputAction -> [(Int, App())]
mkIoOpcodes halt input output = [(99, haltOp halt)
                                ,(3, inputOp input)
                                ,(4, outputOp output)
                                ]

data IntcodeException = OpcodeMissing Int deriving (Show)
instance Exception IntcodeException

mkOpcodesTable :: [(Int, App ())] -> OpcodeTable
mkOpcodesTable opcodes = runSTArray $ do
  let maxCode = maximum (fst <$> opcodes)
      minCode = minimum (fst <$> opcodes)
  table <- newArray (minCode, maxCode) (liftIO $ throwIO $ OpcodeMissing 0)
  forM_ [minCode..maxCode] $ \opcode -> writeArray table opcode (liftIO $ throwIO $ OpcodeMissing opcode)
  forM_ opcodes $ \(opcode, action) -> writeArray table opcode action
  pure table

mkListInput :: [MachineWord] -> IO (IO MachineWord)
mkListInput elements = do
  ref <- newIORef elements
  pure $ do
    elements <- readIORef ref
    case elements of
      (x:xs) -> do
        writeIORef ref xs
        pure x
      _ ->
        error "Ran out of input"

mkListOutput :: IO (IORef [MachineWord], MachineWord -> IO ())
mkListOutput = do
  ref <- newIORef []
  let appender x = do
        modifyIORef ref (x:)
  pure $ (ref, appender)
