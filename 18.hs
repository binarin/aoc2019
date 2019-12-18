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
-- {-# LANGUAGE Strict #-}
module Main where

import Data.List (sortOn)
import Debug.Trace
import GHC.Stack
import Control.Monad (forM, when, foldM)
import Data.Array.ST
import Data.Array
import Control.Lens hiding (Empty)
import Control.Lens.TH
import qualified Data.Set as S
import qualified Data.PSQueue as PSQ
import Data.PSQueue (Binding((:->)))
import Data.Char (toLower)

data Cell = Empty | Wall | Entrance | Key Char | Door Char deriving (Eq, Show)


data Map = Map { _mapWidthL :: Int
               , _mapHeightL :: Int
               , _mapCellsL :: Array Int Cell
               } deriving (Show)

type Coord = Int
type KeySet = S.Set Char
type Point = (Coord, Coord)

makeFields ''Map

fromChar :: Char -> Cell
fromChar '.' = Empty
fromChar '#' = Wall
fromChar '@' = Entrance
fromChar c
  | c >= 'a' && c <= 'z' = Key c
  | c >= 'A' && c <= 'Z' = Door (toLower c)
fromChar c = error $ "Unparsable char " ++ [c]

parseMap :: String -> Map
parseMap input =
  let ls = lines input
      width = length (head ls)
      fullLines = filter ((==width).length) ls
      cells = concatMap (fmap fromChar) fullLines
      height = length fullLines
  in Map width height (listArray (0,width*height-1) cells)

getCell :: Coord -> Coord -> Map -> Cell
getCell x y m =
  if x < 0 || x > m^.widthL-1 || y < 0 || y > m^.heightL -1
  then Wall
  else (m^.cellsL) ! (y * m^.widthL + x)

getCell' :: Point -> Map -> Cell
getCell' = uncurry getCell

findCell :: Map -> Cell -> Point
findCell m c = (x, y)
  where idx = fst $ head $ filter ((==c).view _2) (assocs (m^.cellsL))
        x = idx `rem` (m^.widthL)
        y = idx `div` (m^.widthL)

getCoord :: Int -> Map -> Point
getCoord idx m = (idx `rem` (m^.widthL), idx `div` (m^.widthL))

getIndex :: Point -> Map -> Int
getIndex (x, y) m = (y * m^.widthL + x)

reachableKeys :: Map -> KeySet -> Point -> [(Char, Point, Int)]
reachableKeys m ks pt =
  let wantedKey (idx, (cost, True)) =
        case getCell' (getCoord idx m) m of
          Key c -> not $ S.member c ks
          _ -> False
      wantedKey _ = False
      extract (idx, (cost, _)) =
        let pt = (getCoord idx m)
            Key c = getCell' pt m
        in (c, pt, cost)
      costs = filter wantedKey (assocs (reachability m ks pt))

  in extract <$> costs


reachability :: Map -> KeySet -> Point -> Array Int (Int, Bool)
reachability m ks pt = runSTArray $ do
  state <- newArray (bounds (m^.cellsL)) (maxBound, False)
  let isVisited i = snd <$> readArray state i
      markVisited i = do
        (cost, _) <- readArray state i
        writeArray state i (cost, True)

      getCost i = fst <$> readArray state i
      setCost i newCost = do
        (_, visited) <- readArray state i
        writeArray state i (newCost, visited)

      isReachable i = case getCell' (getCoord i m) m of
                        Empty -> True
                        Wall -> False
                        Key _ -> True
                        Entrance -> True
                        Door c -> S.member c ks

      cells = m^.cellsL

      initialQueue = PSQ.singleton (getIndex pt m) 0

      go q =
        case PSQ.minView q of
          Nothing -> pure ()
          Just (idx :-> cost, q') -> do
            let (x, y)  = getCoord idx m
                adjacent = flip getIndex m <$> [(x, y-1), (x, y+1), (x-1, y), (x+1,y)]
                reachable = filter isReachable adjacent
                targetCost = cost + 1
                tryCell q idx = do
                  thisCost <- getCost idx
                  thisVisited <- isVisited idx
                  if thisVisited || thisCost <= targetCost
                    then pure q
                    else pure $ PSQ.insert idx targetCost q

            markVisited idx
            setCost idx cost

            foldM tryCell q' reachable >>= go

  go initialQueue
  pure state

allKeyMoves :: String -> Map -> KeySet -> Point -> Int -> Int
allKeyMoves level m ks pt bound
  | bound < 0 = 0
  | otherwise =
      let candidates = (\(_, _, cst) -> cst) `sortOn` reachableKeys m ks pt
          descend (ch, pt, cost) bound =
            let subCost = allKeyMoves (ch:level) m (S.insert ch ks) pt (bound - cost)
            in -- ("Subcost", ch, reverse level, subCost) `traceShow`
              cost + subCost
          pickSmallest candidate minCost = min (descend candidate minCost) minCost

      in if null candidates
         then -- ("base", reverse level, ks, bound) `traceShow`
           0
         else (level, ks, bound) `traceShow` foldr pickSmallest bound candidates


main :: IO ()
main = do
  parsed <- parseMap <$> readFile "18.txt"
  -- print parsed
  let entrance = findCell parsed Entrance
  putStrLn $ "Entrace at " ++ show entrance
  print $ reachableKeys parsed S.empty entrance
  -- print $ reachableKeys parsed (S.singleton 'a') (17, 1)
  print $ allKeyMoves "" parsed S.empty entrance maxBound
  pure ()
