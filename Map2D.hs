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
                   } deriving (Eq)

parseMap2D :: a -> (Char -> a) -> ByteString -> Map2D a
parseMap2D pad parse asRead =
  let strings = C8.unpack asRead
      rows = lines strings
      height = length rows
      width = length $ head rows
      padWidth = width + 2
      padHeight = height + 2

      paddingRow = replicate padWidth pad
      parseAndPad r = [pad] ++ (parse <$> r) ++ [pad]
      allRows = [paddingRow] ++ fmap parseAndPad rows ++ [paddingRow]

  in Map padWidth padHeight (V.fromList $ concat allRows)

readMap2D :: a -> (Char -> a) -> FilePath -> IO (Map2D a)
readMap2D pad parse path = do
  parseMap2D pad parse <$> B.readFile path

showMap2D :: (a -> String) -> Map2D a -> String
showMap2D conv (Map w h cs) =
  let showRow y = concat [ conv (cs ! (x + y * w)) | x <- [0..w-1] ] ++ "\n"
  in concatMap showRow [0..h-1]

stripPadding :: Map2D a -> Map2D a
stripPadding (Map w h cs) = runST $ do
  cs' <- MV.new ((w - 2) * (h - 2))
  numLoop 1 (h - 2) $ \y -> do
    numLoop 1 (w - 2) $ \x -> do
      MV.write cs' (x - 1 + (y - 1) * (w - 2)) (cs ! (x + y * w))
  Map (w - 2) (h - 2) <$> V.freeze cs'
