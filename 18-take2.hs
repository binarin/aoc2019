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
import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Vector (Vector, (!))
import qualified Data.Set as S
import Data.IORef

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

dijkstraCover :: Map -> Int -> [(Char, Int, KeySet)]
dijkstraCover (Map w h cs) start = runST $ do
  results <- newSTRef []
  seen <- MUV.replicate (w * h) (Bit False)
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
                          Wall -> pure False
                          _ -> (not . unBit) <$> MUV.read seen idx


      visitCell idx cost ks = do
        MUV.write seen idx (Bit True)

        when (isKey (cs ! idx)) $ do
            let Key ch = cs ! idx
            modifySTRef' results ((ch, cost, ks):)

        let ks' = case cs ! idx of
                    Door c -> S.insert c ks
                    _ -> ks
            cost' = cost + 1

        adjacent <- filterM isVisitable [idx - 1, idx + 1, idx - w, idx + w]
        forM_ adjacent $ \idx' -> do
          modifySTRef' queueRef (PSQ.insert idx' cost' ks')

        go

  go
  readSTRef results

type Transitions = Vector [(Char, Cost, KeySet)]
type TransIdx = Int

type Cost = Int
type Coord = Int

poiToTransIndex :: (Coord, Cell) -> TransIdx
poiToTransIndex (_, Key c) = charToTransIndex c
poiToTransIndex (_, Entrance) = 0
poiToTransIndex poi = error $ "That's not a POI: " ++ show poi

charToTransIndex :: Char -> Int
charToTransIndex c = 1 + ord c - ord 'a'

ksToInt :: KeySet -> Int
ksToInt ks = go 0 ['a'..'z']
  where
    go num [] = num
    go num (c:cs) = go (num * 2 + if S.member c ks then 1 else 0) cs

mkSearchKey :: Char -> KeySet -> Int
mkSearchKey c ks = ksToInt ks * 32 + ord c - ord 'a' + 1

allKeysCost :: Map -> Transitions -> Coord -> IO Cost
allKeysCost (Map _ _ m) trns idx = do
  putStrLn "Allocating seen..."
  seen <- MV.replicate (2^31) (Bit False)
  putStrLn "Done"

  seenCount <- newIORef 0
  qRef <- newIORef $ PSQ.singleton (mkSearchKey '`' S.empty) 0 (S.empty)

  let go = do
        diag
        q <- readIORef qRef
        case PSQ.minView q of
          Nothing -> pure ()
          Just (searchKey, cost, ks, q') -> do
            writeIORef qRef q'
            case S.size ks of
              x | x >= 25 -> print cost
              _ -> visitNode searchKey cost ks

      visitNode searchKey cost ks = do
        MV.write seen searchKey (Bit True)
        modifyIORef' seenCount succ
        let keyNum = searchKey `mod` 32
            keyChar = chr $ ord 'a' + keyNum - 1
            ks' = if keyNum == 0 then ks else S.insert keyChar ks

        candidates <- filterM (isValidTransition ks') (trns ! keyNum)
        forM_ candidates $ \(cCh, cCost, cReqs) -> do
          modifyIORef' qRef (PSQ.insert (mkSearchKey cCh ks') (cost + cCost) ks')

        go

      isValidTransition has (c, _, req) = do
        case S.size (S.difference req has) of
          0 -> (not . unBit) <$> MV.read seen (mkSearchKey c has)
          _ -> pure False


      diag = do
        cnt <- readIORef seenCount
        when ((cnt `rem` 1000) == 0) $ do
          q <- readIORef qRef
          let qSize = PSQ.size q
              Just (_, _, ks) = PSQ.findMin q
          putStrLn $ "Visited " ++ show cnt ++ ", queue " ++ show qSize ++ ", ks " ++ show ks

  go
  pure 0

main :: IO ()
main = do
  m <- addPadding Wall <$> readMap2D fromChar "18-sample3.txt"

  -- putStr $ showMap2D toChar (optimizeMap m)

  let pois = pointsOfInterest m
      entrance = head [ x | (x, Entrance) <- pois ]
      transitions :: Transitions = runST $ do
        ts <- MV.new 27
        forM_ pois $ \poi@(coord, _) -> do
          let idx = poiToTransIndex poi
          MV.write ts idx (dijkstraCover m coord)
        V.freeze ts

  -- print $ sort $ transitions ! 0
  -- print $ searchKey 'z' (S.fromList "abcdefghijklmnopqrstuvwxyz")
  allKeysCost m transitions entrance >>= print

  pure ()
