{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import IntCode
import Control.Monad.IO.Class
import Data.IORef
import Control.Loop
import qualified Data.Vector.Mutable as MV
import Data.Vector.Mutable (MVector)
import Control.Monad (when)
import Control.Monad.Extra (loopM)

isAffected :: Integral a => [MachineWord] -> a -> a -> IO Bool
isAffected code x y = do
  let haltAction = pure ()
  inputAction <- mkListInput [fromIntegral x, fromIntegral y]
  (outputRef, outputAction) <- mkListOutput
  let opcodes = mkOpcodesTable (pureOpcodes ++ mkIoOpcodes haltAction inputAction outputAction)

  vm <- makeRunnable code opcodes
  runIntcode vm runVM
  out <- readIORef outputRef
  pure $ head out > 0

main :: IO ()
main = do
  code <- readProgram "19.txt"

  let squareSize = 100

  let startLine :: Int = 6

      fullScan x Nothing = isAffected code x startLine >>= \case
        True -> fullScan (x + 1) (Just x)
        False -> fullScan (x + 1) Nothing
      fullScan x (Just start) = isAffected code x startLine >>= \case
        True -> fullScan (x + 1) (Just start)
        False -> pure $ (start, x - 1)

      scan x y aff = isAffected code x y >>= \case
        aff'
          | aff' == aff -> pure x
          | otherwise -> scan (x + 1) y aff

  (startL, startR) <- fullScan 0 Nothing

  ctr <- newIORef 2

  rights <- MV.new squareSize
  numLoop 0 (MV.length rights - 1) $ \i -> MV.write rights i 0

  let pushRight y x = do
        ptr <- MV.read rights (squareSize - 1)
        let ptr' = (ptr + 1) `rem` (squareSize - 1)
        goingOut <- MV.read rights ptr'
        MV.write rights ptr' x
        MV.write rights (squareSize - 1) ptr'
        pure goingOut

  (sx, sy) <- flip loopM (startLine + 1, startL, startR) $ \(y, pl, pr) -> do

    newL <- scan pl y True
    newR <- pred <$> scan (max newL pr) y False

    -- putStr $ replicate newL '.'
    -- putStr $ replicate (newR - newL + 1) '#'
    -- putStr "   "

    above100 <- pushRight y newR

    -- print ("Y", y, "NEWR", newR, "AB", above100)

    if above100 - newL + 1 == squareSize
      then pure $ Right (newL, y - squareSize + 1)
      else pure $ Left (y + 1, newL, newR)

  print $ sx * 10000 + sy
  -- numLoop startLine (squareSize-1) $ \y -> do
  --   prevBeam <- readIORef prevBeamRef

  --   numLoop 0 (squareSize - 1) $ \x -> do
  --     aff <- isAffected code x y
  --     putChar (if aff then '#' else '.')
  --     when aff $ modifyIORef ctr succ

  --   putStrLn ""

  -- putStrLn ""
  -- readIORef ctr >>= print

  pure ()
