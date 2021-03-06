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
import Control.Lens hiding (Empty)
import Data.List
import Data.Char
import qualified Data.IntPSQ as PSQ
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed.Mutable as MUV
import Data.Vector (Vector, (!))
import qualified Data.Set as S
import Data.IORef
import qualified Data.PSQueue as PSQueue
import Data.PSQueue (Binding((:->)))

import Map2D

data Cell = Empty | Wall | Entrance Int | Key Char | Door Char deriving (Eq, Show, Ord)

type Map = Map2D Cell
type KeySet = S.Set Char

toChar Empty = " "
toChar Wall = "█"
toChar (Entrance _) = "@"
toChar (Key c) = [c]
toChar (Door c) = [toUpper c]

fromChar :: Char -> Cell
fromChar '.' = Empty
fromChar '#' = Wall
fromChar '@' = Entrance 0
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
    interesting (_, Entrance _) = True
    interesting (_, Key _) = True
    interesting _ = False

dijkstraCover :: Map -> Int -> IO [(Cell, Int, KeySet)]
dijkstraCover (Map w h cs) start = do
  results <- newIORef []
  seen <- MUV.replicate (w * h) (Bit False)
  queueRef <- newIORef $ PSQ.singleton start 0 (S.empty)

  let go = do
        q <- readIORef queueRef
        case PSQ.minView q of
          Nothing -> pure ()
          Just (idx, cost, ks, q') -> do
            -- print ("Popped", idx, cs ! idx, cost, ks)
            writeIORef queueRef q'
            visitCell idx cost ks

      isKey (Key _) = True
      isKey _ = False

      isVisitable idx = case cs ! idx of
                          Wall -> pure False
                          _ -> (not . unBit) <$> MUV.read seen idx


      visitCell idx cost ks = do
        MUV.write seen idx (Bit True)

        when ((isKey (cs ! idx)) && cost > 0) $ do
            let Key ch = cs ! idx
            modifyIORef' results ((Key ch, cost, ks):)

        let ks' = case cs ! idx of
                    Door c -> S.insert c ks
                    _ -> ks
            cost' = cost + 1

        adjacent <- filterM isVisitable [idx - 1, idx + 1, idx - w, idx + w]
        forM_ adjacent $ \idx' -> do
          modifyIORef' queueRef (PSQ.insert idx' cost' ks')

        go

  go
  readIORef results

type Transitions = Vector [(Cell, Cost, KeySet)]
type TransIdx = Int

type Cost = Int
type Coord = Int

poiToTransIndex :: (Coord, Cell) -> TransIdx
poiToTransIndex (_, Key c) = charToTransIndex c
poiToTransIndex (_, Entrance 0) = 0
poiToTransIndex (_, Entrance eNo) = 26 + eNo
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

cellToTrnIdx :: Cell -> Int
cellToTrnIdx (Entrance 0) = 0
cellToTrnIdx (Entrance n) = 26 + n
cellToTrnIdx (Key c) = ord c - ord 'a' + 1
cellToTrnIdx cl = error $ show cl ++ " is not a POI"


addKeyToKs :: Cell -> KeySet -> KeySet
addKeyToKs (Key c) ks = S.insert c ks
addKeyToKs _ ks = ks

isKeyInKs :: Cell -> KeySet -> Bool
isKeyInKs (Key c) ks = S.member c ks
isKeyInKs _ _ = False


allKeysCost :: Int -> Map -> Transitions -> Coord -> IO Cost
allKeysCost reqCnt (Map _ _ m) trns idx = do
  qRef <- newIORef $ PSQueue.singleton (Entrance 0, S.empty :: KeySet) 0
  cntRef :: IORef Int <- newIORef 0

  let go = do
        diag
        c <- readIORef cntRef
        when (c > 10) $ do
          undefined
        modifyIORef' cntRef succ

        q <- readIORef qRef
        case PSQueue.minView q of
          Nothing -> error $ "Search space exhausted " ++ show reqCnt
          Just ((cell, ks) :-> cost, q') -> do
            writeIORef qRef q'
            case S.size ks of
              sz | sz == reqCnt - 1 -> pure cost
              _ -> do
                print $ ("At", cell, ks, cost)
                visitCell cell ks cost
                if (ks == S.fromList "")
                  then go
                  else error "ho"

      visitCell cell ks cost = do
        modifyIORef' cntRef succ
        let ks' = addKeyToKs cell ks
            candidates = filter (isValidTransition ks') (trns ! (cellToTrnIdx cell))
        -- print $ ("Cand", candidates, (trns ! (cellToTrnIdx cell)))
        forM_ candidates $ \(target, tCost, _) -> do
          modifyIORef' qRef (PSQueue.insertWith min (target, ks') (cost + tCost))


      isValidTransition has (target, _, req) =
        case isKeyInKs target has of
          True -> False
          False -> 0 == S.size (S.difference req has)

      diag = do
        cnt <- readIORef cntRef
        when ((cnt `rem` 1000) == 0) $ do
          q <- readIORef qRef
          print (cnt, PSQueue.size q)

  go

main = part2

enumerateEntrances :: Map -> Map
enumerateEntrances (Map w h cs) = runST $ do
  new <- V.thaw cs
  numRef <- newSTRef 0
  numLoop 0 (V.length cs - 1) $ \idx ->
    case cs ! idx of
      Entrance _ -> do
        cur <- readSTRef numRef
        modifySTRef' numRef succ
        MV.write new idx (Entrance cur)
      _ -> pure ()

  Map w h <$> V.freeze new

part2 :: IO ()
part2 = do
  m <- enumerateEntrances . addPadding Wall <$> readMap2D fromChar "18-2.txt"
  let pois = pointsOfInterest m
      keysNum = foldr (+) 0 [ 1 | (_, Key _) <- pois ]

  transitions :: Transitions <- do
        ts <- MV.new 30
        forM_ pois $ \poi@(coord, _) -> do
          let idx = poiToTransIndex poi
          cover <- dijkstraCover m coord
          MV.write ts idx cover
        V.freeze ts

  qRef <- newIORef $ PSQueue.singleton (Entrance 0, Entrance 1, Entrance 2, Entrance 3, S.empty :: KeySet) 0
  cntRef <- newIORef 0

  let go = do
        modifyIORef cntRef succ
        q <- readIORef qRef
        case PSQueue.minView q of
          Nothing -> error "Search queue exhausted"
          Just (state@(_, _, _, _, ks) :-> cost, q') -> do
            when (S.size ks == keysNum) $ do
              error $ show cost
            cnt <- readIORef cntRef
            when (cnt `mod` 1000 == 0) $ do
              print $ (cnt, cost, ks, S.size ks)
            writeIORef qRef q'
            -- putStrLn "\n--------------------"
            -- print $ ("At", cost, state)
            -- print "1"
            moveRobot (_1) state cost
            -- print "2"
            moveRobot (_2) state cost
            -- print "3"
            moveRobot (_3) state cost
            -- print "4"
            moveRobot (_4) state cost
            q <- readIORef qRef
            -- putStrLn "===================="
            -- sequence $ print <$> sort (PSQueue.toList q)
            go

      moveRobot :: ALens' (Cell, Cell, Cell, Cell, KeySet) Cell -> (Cell, Cell, Cell, Cell, KeySet) -> Int -> IO ()
      moveRobot lens state@(_, _, _, _, ks) cost = do
        let cell = state^#lens
            ks' = addKeyToKs cell ks
            trns = transitions ! (cellToTrnIdx cell)
            candidates = filter (isValidTransition ks') trns
        -- print $ ks'
        -- print $ candidates
        -- print $ trns
        forM_ candidates $ \(target, tCost, _) -> do
          let state' = state & lens #~ target
                             & _5 #~ (addKeyToKs target ks')
          modifyIORef' qRef (PSQueue.insertWith min state' (cost + tCost))
        pure ()

      isValidTransition has (target, _, req) =
        case isKeyInKs target has of
          True -> False
          False -> 0 == S.size (S.difference req has)

  go
  -- print $ sort $ transitions ! 27
  pure ()

part1 :: IO ()
part1 = do
  m <- addPadding Wall <$> readMap2D fromChar "18.txt"

  -- putStr $ showMap2D toChar (optimizeMap m)

  let pois = pointsOfInterest m
      entrance = head [ x | (x, Entrance 0) <- pois ]
      keysNum = foldr (+) 0 [ 1 | (_, Key _) <- pois ]

  transitions :: Transitions <- do
        ts <- MV.new 27
        forM_ pois $ \poi@(coord, _) -> do
          let idx = poiToTransIndex poi
          cover <- dijkstraCover m coord
          MV.write ts idx cover
        V.freeze ts

  -- sort <$> dijkstraCover m entrance >>= print
  print $ sort $ transitions ! 0
  -- print $ searchKey 'z' (S.fromList "abcdefghijklmnopqrstuvwxyz")
  print ("Poi", pois)
  print ("KNum", keysNum)
  allKeysCost keysNum m transitions entrance >>= print

  pure ()
