{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Map2D where

import Debug.Trace
import Data.Maybe (isJust, fromJust)
import Control.Monad (when, filterM, forM_)
import qualified Data.PSQueue as PSQ
import Data.PSQueue (Binding((:->)))
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST (runST)
import Control.Loop (numLoop)
import Control.Lens
import Control.Lens.TH
import Data.STRef


data Map2D a = Map { _mapWidthL :: Int
                   , _mapHeightL :: Int
                   , _mapCellsL :: Vector a
                   } deriving (Eq, Show)

makeFields ''Map2D

parseMap2D :: (Char -> a) -> ByteString -> Map2D a
parseMap2D parse asRead =
  let strings = C8.unpack asRead
      rows = lines strings
      height = length rows
      width = length $ head rows
  in Map width height (V.fromList $ concat $ (fmap parse) <$> rows)

readMap2D :: (Char -> a) -> FilePath -> IO (Map2D a)
readMap2D parse path = do
  parseMap2D parse <$> B.readFile path

showMap2D :: (a -> String) -> Map2D a -> String
showMap2D conv (Map w h cs) =
  let showRow y = concat [ conv (cs ! (x + y * w)) | x <- [0..w-1] ] ++ "\n"
  in concatMap showRow [0..h-1]


addPadding :: a -> Map2D a -> Map2D a
addPadding pad (Map w h cs) = runST $ do
  cs' <- MV.replicate ((w + 2) * (h + 2)) pad
  numLoop 0 (h - 1) $ \y -> do
    numLoop 0 (w - 1) $ \x -> do
      MV.write cs' (x + 1 + (y + 1) * (w + 2)) (cs ! (x + y * w))
  Map (w + 2) (h + 2) <$> V.freeze cs'

stripPadding :: Map2D a -> Map2D a
stripPadding (Map w h cs) = runST $ do
  cs' <- MV.new ((w - 2) * (h - 2))
  numLoop 1 (h - 2) $ \y -> do
    numLoop 1 (w - 2) $ \x -> do
      MV.write cs' (x - 1 + (y - 1) * (w - 2)) (cs ! (x + y * w))
  Map (w - 2) (h - 2) <$> V.freeze cs'

findCells :: (a -> Bool) -> Map2D a -> [(a, Int)]
findCells f (Map w h cs) = V.toList $ V.filter (\(c, _) -> f c) $ V.imap (\idx c -> (c, idx)) cs

type Cost = Int
type MapIndex = Int

dijkstraCover :: (a -> Bool) -> (a -> Bool) -> MapIndex -> Map2D a -> [(MapIndex, Cost)]
dijkstraCover isInteresting isVisitable startIdx (Map w h cs) = runST $ do
  seen <- MV.replicate (w * h) False
  qRef <- newSTRef $ PSQ.singleton startIdx 0
  resultRef <- newSTRef []

  let go = do
        q <- readSTRef qRef
        case PSQ.minView q of
          Nothing -> pure ()
          Just (idx :-> cost, q') -> do
            writeSTRef qRef q'
            MV.write seen idx True
            when (idx /= startIdx && isInteresting (cs ! idx)) $ do
              modifySTRef' resultRef ((idx, cost):)
            visitCell idx cost
            go

      visitCell idx cost = do
        let (y, x) = idx `divMod` w
            up = if y > 0 then Just (idx - w) else Nothing
            down = if y < h - 1 then Just (idx + w) else Nothing
            left = if x > 0 then Just (idx - 1) else Nothing
            right = if x < w - 1 then Just (idx + 1) else Nothing
            validDirections = fromJust <$> filter isJust [up, down, left, right]
            candidates = filter (isVisitable . (cs!)) validDirections
        unseen <- filterM (fmap not . MV.read seen) candidates
        forM_ unseen $ \target -> do
          modifySTRef' qRef (PSQ.insertWith min target (cost + 1))
        pure ()

  go

  readSTRef resultRef
