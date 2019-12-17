{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Debug.Trace
import Control.Lens
import Control.Lens.TH
import IntCode
import Control.Monad.IO.Class
import Data.IORef
import Data.Char
import Data.Array.Unboxed

data Direction = North | East | South | West deriving (Show)

data Map = Map { _mapWidthL :: Int
               , _mapHeightL :: Int
               , _mapCellsL :: Array Int Bool
               } deriving (Show)

makeFields ''Map

hasScaffold :: Char -> Bool
hasScaffold 'X' = False
hasScaffold '.' = False
hasScaffold _ = True

sampleMap :: String
sampleMap = concat [ "..#..........\n"
                   , "..#..........\n"
                   , "#######...###\n"
                   , "#.#...#...#.#\n"
                   , "#############\n"
                   , "..#...#...#..\n"
                   , "..#####...^..\n"
                   ]

parseMap :: String -> Map
parseMap input =
  let ls = lines input
      width = length (head ls)
      fullLines = filter ((==width).length) ls
      scaffolds :: [Bool] = concatMap (fmap hasScaffold) fullLines
      height = traceShowId $ length fullLines
  in Map width height (listArray (0,width*height-1) scaffolds)

getCell :: Int -> Int -> Map -> Bool
getCell x y m =
  let w = m^.widthL
      h = m^.heightL
      cells = m^.cellsL
      maxX = w - 1
      maxY = h - 1
  in if x > maxX || x < 0 || y > maxY || y < 0
     then False
     else cells ! (x + y * w)

isIntesection :: Map -> Int -> Int -> Bool
isIntesection m x y = and ((\(x, y) -> getCell x y m) <$> adjacent)
  where
    adjacent = [(x, y), (x, y - 1), (x, y + 1), (x - 1, y), (x + 1, y)]

getIntersections :: Map -> [(Int, Int)]
getIntersections m = [ (x, y) | y <- [0..m^.heightL-1], x <- [0..m^.widthL-1], isIntesection m x y ]

main :: IO ()
main = do
  prog <- readProgram "17.txt"

  inputAction <- mkListInput []
  (outputRef, outputAction) <- mkListOutput

  let haltAction = liftIO $ putStrLn "Halted"
      opcodes = mkOpcodesTable (pureOpcodes ++ mkIoOpcodes haltAction inputAction outputAction)

  vm <- makeRunnable prog opcodes
  runIntcode vm runVM

  -- parsed <- parseMap . reverse . fmap (chr.fromIntegral) <$> readIORef outputRef
  raw <- reverse . fmap (chr . fromIntegral) <$> readIORef outputRef
  putStrLn raw

  let parsed = parseMap raw
  -- print $ length $ lines raw
  -- sequence $ print <$> (length <$> lines raw)
  -- print $ parsed^.widthL
  -- print $ parsed^.heightL
  print $ sum $ uncurry (*) <$> getIntersections parsed

  pure ()
