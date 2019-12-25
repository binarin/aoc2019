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

import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST (runST)
import Control.Loop (numLoop)


data Map2D a = Map { _mapWidthL :: Int
                   , _mapHeightL :: Int
                   , _mapCellsL :: Vector a
                   } deriving (Eq, Show)

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
