{-# LANGUAGE Strict #-}
module Main where

import Debug.Trace
import Control.Monad
import Map2D
import Control.Loop
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import Control.Monad.ST
import Utils

type Map = Map2D Bool

parseCell '#' = True
parseCell '.' = False
parseCell c = error $ "Invalid char " ++ show c

printCell True = "#"
printCell False = "."

golIteration :: Map -> Map
golIteration (Map w h cs) =
  let cs' = runST $ do
        new <- MV.replicate (w*h) False
        numLoop 1 (h-2) $ \y -> do
          numLoop 1 (w-2) $ \x -> do
            let idx = x + y * w
                adj = [ idx - 1, idx + 1, idx - w, idx + w ]
                adjBugs = length $ filter (==True) ((cs !) <$> adj)
                bug = case cs ! idx of
                        True -> adjBugs == 1
                        False -> adjBugs == 1 || adjBugs == 2
            MV.write new idx bug
        V.freeze new
  in Map w h cs'

--  #
-- #.#
--  #

adjacentCount :: Int -> Vector Bool -> (Bool, Int)
adjacentCount idx cs =
  let size = V.length cs
      totalLevels = size `div` 25
      (level, idxLev) = idx `divMod` 25
      (y, x) = idxLev `divMod` 5

      up level _ 0 = [ (level - 1, 2, 1) ]
      up level 2 3 = [ (level + 1, 0, 4)
                     , (level + 1, 1, 4)
                     , (level + 1, 2, 4)
                     , (level + 1, 3, 4)
                     , (level + 1, 4, 4)
                     ]
      up level 2 2 = error $ "Up on recursive cell at level " ++ show level
      up level x y = [ (level, x, y - 1) ]

      down level _ 4 = [ (level - 1, 2, 3) ]
      down level 2 1 = [ (level +1, 0, 0)
                       , (level +1, 1, 0)
                       , (level +1, 2, 0)
                       , (level +1, 3, 0)
                       , (level +1, 4, 0)
                       ]
      down level 2 2 = error $ "Down on recursive cell at level " ++ show level
      down level x y = [ (level, x, y + 1) ]

      left level 0 _ = [ (level - 1, 1, 2) ]
      left level 3 2 = [ (level + 1, 4, 0)
                       , (level + 1, 4, 1)
                       , (level + 1, 4, 2)
                       , (level + 1, 4, 3)
                       , (level + 1, 4, 4)
                       ]
      left level 2 2 = error $ "Left on recursive cell at level " ++ show level
      left level x y = [ (level, x - 1, y) ]

      right level 4 _ = [ (level - 1, 3, 2) ]
      right level 1 2 = [ (level + 1, 0, 0)
                        , (level + 1, 0, 1)
                        , (level + 1, 0, 2)
                        , (level + 1, 0, 3)
                        , (level + 1, 0, 4)
                        ]
      right level 2 2 = error $ "Right on recursive cell at level " ++ show level
      right level x y = [ (level, x + 1, y) ]

      readAt (level, x, y)
        | level < 0 = False
        | level >= totalLevels = False
        | otherwise = cs ! (level * 25 + y * 5 + x)

      readAt' = boolToInt . readAt

      boolToInt True = 1
      boolToInt False = 0

      us = sum $ readAt' <$> up level x y
      ds = sum $ readAt' <$> down level x y
      ls = sum $ readAt' <$> left level x y
      rs = sum $ readAt' <$> right level x y

  in (readAt (level, x, y), us + ds + ls + rs)

addIfTrue True acc = acc + 1
addIfTrue False acc = acc

recursiveGol :: Vector Bool -> Vector Bool
recursiveGol cs = runST $ do
  let levels = V.length cs `div` 25
  extended <- MV.replicate (V.length cs + 50) False
  numLoop 0 (V.length cs + 50 - 1) $ \idx -> when (idx `rem` 25 /= 12) $ do
    let (alive, adjCount) = adjacentCount (idx - 25) cs
        alive' = case alive of
          True -> adjCount == 1
          False -> adjCount == 1 || adjCount == 2
    MV.write extended idx alive'

  full <- V.freeze extended

  let topBugs = V.foldr' addIfTrue 0 (V.slice 0 25 full)
      adjustTop (lower, cnt) = if topBugs == 0
                               then (lower + 25, cnt - 25)
                               else (lower, cnt)
      bottomBugs = V.foldr' addIfTrue 0 (V.slice (V.length cs + 25) 25 full)
      adjustBottom (lower, cnt) = if bottomBugs == 0
                                  then (lower, cnt - 25)
                                  else (lower, cnt)
      (start, cnt) = adjustBottom (adjustTop (0, V.length cs + 50))

  pure $ V.slice start cnt full

biodiversity :: Map -> Integer
biodiversity (Map _ _ cs) = go 0 0 1
  where
    until = V.length cs
    go idx bd power2
      | idx >= until = bd
      | cs ! idx = go (idx + 1) (bd + power2) (power2 * 2)
      | otherwise = go (idx + 1) bd (power2 * 2)

main :: IO ()
main = do
  part2
  pure ()

printRecMap :: Vector Bool -> IO ()
printRecMap cs = do
  let size = V.length cs
      totalLevels = size `div` 25
  numLoop 0 (totalLevels-1) $ \level -> do
    putStrLn $ "==================== " ++ show level
    numLoop 0 4 $ \y -> do
      numLoop 0 4 $ \x -> do
        putStr $ printCell $ (cs ! (level * 25 + y * 5 + x))
      putStrLn ""

part2 :: IO ()
part2 = do
  (Map _ _ sample) <- readMap2D False parseCell "24-sample.txt"
  let sample10 = head $ drop 10 $ iterate recursiveGol sample
  print $ V.foldr' addIfTrue 0 sample10

  (Map _ _ full) <- readMap2D False parseCell "24.txt"
  let full200 = head $ drop 200 $ iterate recursiveGol full
  print $ V.foldr' addIfTrue 0 full200


  -- putStrLn $ showMap2D printCell $ golIteration $ addPadding False m'

  -- print $ adjacentCount (-24) m

  -- printRecMap m


part1 :: IO ()
part1 = do
  m <- addPadding False <$> readMap2D False parseCell "24.txt"

  putStrLn $ showMap2D printCell $ m

  let mkSteps () = iterate golIteration m
      --       1 2s   2   4s
      go dist (t:ts) hare@(h:_:hs)
        | t == h = (dist, hare)
        | otherwise = go (dist+1) ts hs

      (v, hare)  = go 1 (tail $ mkSteps ()) (drop 2 $ mkSteps ())

      findRep (t:ts) (h:hs)
        | t == h = t
        | otherwise = findRep ts hs
      firstRep = findRep (mkSteps ()) hare

  putStrLn $ showMap2D printCell $ stripPadding $ firstRep
  print $ biodiversity $ stripPadding firstRep

  -- putStrLn $ showMap2D printCell m
  -- putStrLn $ showMap2D printCell solution
  -- sequence $ (putStrLn . showMap2D printCell) <$> take 1000 steps
  pure ()
