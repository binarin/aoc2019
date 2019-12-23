{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad (when)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class
import IntCode
import Control.Concurrent.STM
import qualified Data.Vector.Mutable as MV
import Control.Loop
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Data.IORef

data OutputState = WaitAddr | WaitX | WaitY deriving (Eq, Show)

type Message = (MachineWord, MachineWord)


nextOutputState WaitAddr = WaitX
nextOutputState WaitX = WaitY
nextOutputState WaitY = WaitAddr


mkNetworkOutput :: TVar Int -> Vector (TChan Message) -> TChan Message -> IO OutputAction
mkNetworkOutput idleTvar nics nat = do
  state <- newIORef WaitAddr

  addrRef :: IORef Int <- newIORef 0
  xRef <- newIORef 0

  let switchToNextState = liftIO $ do
        cur <- readIORef state
        writeIORef state (nextOutputState cur)
        pure cur

  pure $ \word -> do
    liftIO $ atomically $ writeTVar idleTvar 0
    switchToNextState >>= \case
      WaitAddr -> liftIO $ do
        writeIORef addrRef (fromIntegral word)
      WaitX -> liftIO $ do
        writeIORef xRef word
      WaitY -> liftIO $ do
        (addr, x) <- (,) <$> readIORef addrRef <*> readIORef xRef
        let ch = case addr of
                   255 -> nat
                   _ -> nics ! addr
        atomically $ writeTChan ch (x, word)
        pure ()

mkNetworkInput :: TVar Int -> MachineWord -> TChan Message -> IO InputAction
mkNetworkInput idleTvar nicId chan = do
  stateRef <- newIORef (Just nicId)

  pure $ liftIO $ do
    readIORef stateRef >>= \case
      Nothing -> do
        empty <- atomically $ isEmptyTChan chan
        case empty of
          True -> do
            threadDelay 1000
            atomically $ modifyTVar' idleTvar (+1)
            pure (-1)
          False -> do
            (x, y) <- atomically $ readTChan chan
            writeIORef stateRef (Just y)
            pure x
      Just y -> do
        writeIORef stateRef Nothing
        pure y


main :: IO ()
main = do
  let computerCount = 50
  chansM <- MV.new computerCount
  solutionCh <- newTChanIO

  code <- readProgram "23.txt"

  numLoop 0 (computerCount - 1) $ \addr -> do
    ch <- newTChanIO
    MV.write chansM addr ch

  chans <- V.freeze chansM

  idleStatusM <- MV.new computerCount

  numLoop 0 (computerCount - 1) $ \addr -> do
    let inputCh = chans ! addr
        haltAction = liftIO $ pure ()

    idleTvar <- newTVarIO 0
    MV.write idleStatusM addr idleTvar

    inputAction <- mkNetworkInput idleTvar (fromIntegral addr) inputCh

    outputAction <- mkNetworkOutput idleTvar chans solutionCh

    let opcodes = mkOpcodesTable $ pureOpcodes ++ mkIoOpcodes haltAction inputAction outputAction

    vm <- makeRunnable code opcodes
    forkIO $ runIntcode vm runVM

    pure ()

  idleStatus <- V.freeze idleStatusM

  let go lastMsg@(_, lastMsgY) lastYSent = do
        threadDelay 1000
        atomically (tryReadTChan solutionCh) >>= \case
          Just msg@(_, y) -> go msg lastYSent
          Nothing -> do
            allIdle <- atomically $ do
              mapM readTVar (V.toList idleStatus)
            case all (>100) allIdle of
              True -> do
                putStrLn $ "Kicking off with " ++ show lastMsg
                atomically $ writeTChan (chans ! 0) lastMsg
                threadDelay 10000
                if lastMsgY == lastYSent
                  then putStrLn $ "Stopping at " ++ show lastMsgY
                  else go lastMsg lastMsgY
              False -> go lastMsg lastYSent

  go (0, 0) 0

  pure ()
