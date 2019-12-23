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

import Data.STRef
import Debug.Trace
import Control.Loop
import Control.Monad.ST
import Control.Monad
import Control.Monad.Extra
import Data.List
import Data.Char
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Vector (Vector, (!))
import qualified Data.Set as S
import Control.Lens hiding (Empty)
import Control.Lens.TH
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

data Cell = Empty | Wall | Entrance | Key Char | Door Char deriving (Eq, Show)

type Coord = Int
type Width = Coord
type Height = Coord
type KeySet = S.Set Char
type Point = (Coord, Coord)

data Map = Map { _mapWidthL :: Width
               , _mapHeightL :: Height
               , _mapCellsL :: Vector Cell
               }

instance Show Map where
  show (Map w h cells) =
    let showRow y = concat [ toChar (cells ! (x + y * w)) | x <- [0..w-1] ] ++ "\n"
        toChar Empty = " "
        toChar Wall = "█"
        toChar Entrance = "@"
        toChar (Key c) = [c]
        toChar (Door c) = [toUpper c]
      in concatMap showRow [0..h-1]


makeFields ''Map


fromChar :: Char -> Cell
fromChar '.' = Empty
fromChar '#' = Wall
fromChar '@' = Entrance
fromChar c
  | c >= 'a' && c <= 'z' = Key c
  | c >= 'A' && c <= 'Z' = Door (toLower c)
fromChar c = error $ "Unparsable char " ++ [c]

parseMap :: ByteString -> Map
parseMap asRead =
  let strings = C8.unpack asRead
      rows = lines strings
      height = length rows
      width = length $ head rows
      padWidth = width + 2
      padRow = replicate padWidth '#'
      padHeight = height + 2
      paddedRows = (\r -> "#" ++ r ++ "#") <$> rows
      allRows = [padRow] ++ paddedRows ++ [padRow]

  in Map padWidth padHeight (V.fromList $ fromChar <$> concat allRows)

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



main :: IO ()
main = do
  raw <- B.readFile "18.txt"
  let parsed = parseMap raw
  print $ optimizeMap parsed
  pure ()
