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

import Control.Monad (forM, when)
import Data.Array.ST
import Data.Array
import Control.Lens hiding (Empty)
import Control.Lens.TH
import qualified Data.Set as S
import qualified Data.PSQueue as PSQ
import Data.PSQueue (Binding((:->)))

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
  | c >= 'A' && c <= 'Z' = Door c
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
findCell m c = (idx, idx)
  where idx = fst $ head $ filter ((==c).view _2) (assocs (m^.cellsL))
        x = idx `rem` (m^.widthL)
        y = idx `div` (m^.widthL)

getCoord :: Int -> Map -> Point
getCoord idx m = (idx `rem` (m^.widthL), idx `div` (m^.widthL))

getIndex :: Point -> Map -> Int
getIndex (x, y) m = (y * m^.widthL + x)

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
            forM reachable $ \i -> do
              iCost <- getCost i
              when
              pure ()
            pure ()


  go initialQueue


  -- writeArray seen 0 False
  -- writeArray cost 0 0
  -- dijkstra (PSQ.singleton (getIndex pt m) 0)
  pure state



-- reachable :: Map -> KeySet -> Point -> [(Char, Coord, Int)]
-- reachable m ks coord = undefined
--   where
--     go st@(seen, found, xy, cost) =
--       case getCell' xy m of
--         Key c -> handleKey c st
--         Door c -> handleDoor c st
--         Wall -> st
--         Empty -> resumeSearh st
--         Entrance -> resumeSearh st

--     handleKey ch st@(seen, found, xy, cost) = undefined
--     handleDoor ch st@(seen, found, xy, cost) =
--       case S.member ch ks of
--         True ->

--     resumeSearh st@(seen, found, xy@(x, y), cost) =
--       let adjacent = [ (x, y-1), (x, y+1), (x-1, y), (x+1, y) ]
--           unseenAdj = filter (flip S.member seen) adjacent
--           seen' = S.insert xy seen
--       in foldr (\xy (seen, found, _, cost) -> go (seen, found, xy, cost + 1)) st unseenAdj

main :: IO ()
main = do
  parsed <- parseMap <$> readFile "18-sample1.txt"
  print parsed
  let entrance = findCell parsed Entrance
  pure ()
