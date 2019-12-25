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
module Main where

import Data.Bit
import Data.STRef
import Debug.Trace
import Control.Loop
import Control.Monad.ST
import Control.Monad
import Control.Monad.Extra
import Data.List
import Data.Char
import qualified Data.IntPSQ as PSQ
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Vector (Vector, (!))
import qualified Data.Set as S

import Map2D

data Cell = Empty | Wall | Entrance | Key Char | Door Char deriving (Eq, Show)

type Map = Map2D Cell
type KeySet = S.Set Char

toChar Empty = " "
toChar Wall = "█"
toChar Entrance = "@"
toChar (Key c) = [c]
toChar (Door c) = [toUpper c]

fromChar :: Char -> Cell
fromChar '.' = Empty
fromChar '#' = Wall
fromChar '@' = Entrance
fromChar c
  | c >= 'a' && c <= 'z' = Key c
  | c >= 'A' && c <= 'Z' = Door (toLower c)
fromChar c = error $ "Unparsable char " ++ [c]

-- so the only loop in the map is the 3x3 empty square near entrance
optimizeMap :: Map -> Map
optimizeMap (Map w h cells) =
  let cells' = runST $ do
        cs <- V.thaw cells
        let toIndex x y = x + y * w
            maybeFixDeadEnd idx = do
              let adjIxs = [idx - 1
                           ,idx + 1
                           ,idx - w
                           ,idx + w
                           ]
              adj <- mapM (MV.read cs) adjIxs
              let (walls, nonWall) = partition (== Wall) adj
              when (length walls == 3) $ do
                MV.write cs idx Wall
                let Just nonWallIdx = (adjIxs !!) <$> findIndex (/= Wall) adj
                case nonWall of
                  [Empty] -> maybeFixDeadEnd nonWallIdx
                  [Door _] -> maybeFixDeadEnd nonWallIdx
                  _ -> pure ()

        whileM $ do
          didOptimize <- newSTRef False
          numLoop 1 (h - 2) $ \y -> do
            numLoop 1 (w - 2) $ \x -> do
              maybeFixDeadEnd (x + y * w)
          readSTRef didOptimize
        V.freeze cs

  in Map w h cells'


pointsOfInterest :: Map -> [(Int, Cell)]
pointsOfInterest (Map _ _ m) = V.toList $ V.filter interesting $ V.indexed m
  where
    interesting (_, Entrance) = True
    interesting (_, Key _) = True
    interesting _ = False

dijkstraCover :: Map -> Int -> [(Cell, Int, KeySet)]
dijkstraCover (Map w h cs) start = runST $ do
  results <- newSTRef []
  seen <- MV.replicate (w * h) (Bit False)
  queueRef <- newSTRef $ PSQ.singleton start 0 (S.empty)

  let go = do
        q <- readSTRef queueRef
        case PSQ.minView q of
          Nothing -> pure ()
          Just (idx, cost, ks, q') -> do
            writeSTRef queueRef q'
            visitCell idx cost ks

      isKey (Key _) = True
      isKey _ = False

      isVisitable idx = case cs ! idx of
                          Wall -> False
                          _ -> True


      visitCell idx cost ks = do
        MV.write seen idx (Bit True)

        when (isKey (cs ! idx)) $ do
            modifySTRef' results ((cs ! idx, cost, ks):)

        let adjacent = filter isVisitable [idx - 1, idx + 1, idx - w, idx + w]

        pure ()

  go
  readSTRef results


main :: IO ()
main = do
  m <- addPadding Wall <$> readMap2D fromChar "18.txt"

  -- putStr $ showMap2D toChar (optimizeMap m)
  print $ pointsOfInterest m
  print $ dijkstraCover m (fst $ head $ pointsOfInterest m)

  pure ()
