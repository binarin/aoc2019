{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Data.Maybe
import Control.Lens hiding (Empty, Level)
import Data.Char
import Control.Concurrent (threadDelay)
import Data.List (sort, findIndex)
import qualified Data.Map as M
import qualified Data.Vector.Mutable as MV
import Control.Monad.ST
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Control.Loop
import Control.Monad.ST
import Control.Monad
import Data.STRef
import Data.IORef
import qualified Data.IntPSQ as PSQ
import qualified Data.HashMap.Strict as HM

import Map2D hiding (Cost)

data RawCell = RawWall | RawEmpty | RawPortalPart Char | RawPortal Char Char
  deriving (Eq, Show)

type Coord = Int
type Point = (Int, Int)

type PortalLabel = (Char, Char)

data PortalType = Inner | Outer deriving (Eq, Show, Ord)
data Cell = Empty | Wall | Portal PortalLabel Point PortalType | Entrance | Exit deriving (Eq, Show, Ord)

type Width = Int
type Height = Int

charToRawCell ' ' = RawWall
charToRawCell '.' = RawEmpty
charToRawCell '#' = RawWall
charToRawCell c
  | c >= 'A' && c <= 'Z' = RawPortalPart c
  | otherwise = error $ "Unknown char '" ++ [c] ++ "'"

mvectorFromList :: [a] -> IO (MV.IOVector a)
mvectorFromList els = do
  cells <- MV.new $ length els
  let populate _ [] = pure ()
      populate i (x:xs) = do
        MV.write cells i x
        populate (i+1) xs
  populate 0 els
  pure cells

readMap :: FilePath -> IO (Width, Height, Vector RawCell)
readMap path = do
  rows <- lines <$> readFile path

  let unpaddedHeight = length rows
      paddedHeight = unpaddedHeight + 2
      unpaddedWidth = length $ head rows
      paddedWidth = unpaddedWidth + 2

  let padRow = replicate paddedWidth ' '
      padColumns s = " " ++ s ++ " "

      paddedRows = [padRow] ++ (padColumns <$> rows) ++ [padRow]

  cells <- mvectorFromList $ charToRawCell <$> concat paddedRows
  result <- V.freeze cells

  pure (paddedWidth, paddedHeight, result)

parseMap :: Width -> Height -> Vector RawCell -> Vector Cell
parseMap w h rawIn = runST $ do
  raw <- V.thaw rawIn

  final <- MV.replicate (w * h) Wall
  portalRef <- newSTRef (M.empty)

  let readRaw x y = MV.read raw (x + y * w)
      writeRaw x y v = do
        MV.write raw (x + y * w) v
        case v of
          RawWall -> pure ()
          RawPortalPart _ -> pure ()
          RawEmpty -> MV.write final (x + y * w) Empty
          RawPortal 'A' 'A' -> MV.write final (x + y * w) Entrance
          RawPortal 'Z' 'Z' -> MV.write final (x + y * w) Exit
          RawPortal a b -> finalizePortal x y (a, b)

      finalizePortal x y label = do
        portals <- readSTRef portalRef
        case M.lookup label portals of
          Nothing -> do
            writeSTRef portalRef (M.insert label (x, y) portals)
          Just (ox, oy) -> do
            let thisIsOuter = ox <= 3 || ox >= w - 4 || oy <= 3 || oy >= h - 4

            MV.write final (ox + oy * w) (Portal label (x, y) (if thisIsOuter then Outer else Inner))
            MV.write final (x + y * w) (Portal label (ox, oy) (if not thisIsOuter then Outer else Inner))
        pure ()

      detectPortal x y labelPart = do
        thisC <- readRaw x y
        upC <- readRaw x (y-1)
        downC <- readRaw x (y+1)
        leftC <- readRaw (x-1) y
        rightC <- readRaw (x+1) y
        case (upC, downC, leftC, rightC) of
          (RawEmpty, RawPortalPart lp, _, _) -> do
            writeRaw x y RawWall
            writeRaw x (y+1) RawWall
            writeRaw x (y-1) (RawPortal labelPart lp)
          (_, RawPortalPart lp, _, _) -> do
            writeRaw x y RawWall
            writeRaw x (y+1) RawWall
            writeRaw x (y+2) (RawPortal labelPart lp)
          (_, _, RawEmpty, RawPortalPart lp) -> do
            writeRaw x y RawWall
            writeRaw (x+1) y RawWall
            writeRaw (x-1) y (RawPortal labelPart lp)
          (_, _, _, RawPortalPart lp) -> do
            writeRaw x y RawWall
            writeRaw (x+1) y RawWall
            writeRaw (x+2) y (RawPortal labelPart lp)

  numLoop 1 (h - 3) $ \y -> do
    numLoop 1 (w - 3) $ \x -> do
      MV.read raw (x + y * w) >>= \case
        RawWall -> pure ()
        RawEmpty -> MV.write final (x + y * w) Empty
        RawPortal _ _ -> pure ()
        RawPortalPart labelPart -> detectPortal x y labelPart

  V.freeze final

printMap :: Width -> Height -> Vector Cell -> IO ()
printMap w h v = do
  let render Wall = "██"
      render Empty = "  "
      render (Portal (a,b) _ Inner) = (toLower a:toLower b:[])
      render (Portal (a,b) _ Outer) = (a:b:[])
      render Entrance = "AA"
      render Exit = "ZZ"

  numLoop 0 (h - 1) $ \y -> do
    numLoop 0 (w - 1) $ \x -> do
      putStr $ render (v ! (x + y * w))
    putStrLn ""

isVisitable :: Cell -> Bool
isVisitable Wall = False
isVisitable _ = True

isInteresting :: Cell -> Bool
isInteresting Wall = False
isInteresting Empty = False
isInteresting _ = True


part2 :: IO ()
part2 = do
  Map w h csRaw <- addPadding RawWall <$> readMap2D charToRawCell "20.txt"
  let m@(Map _ _ cs) = Map w h (parseMap w h csRaw)
      poi = findCells (\c -> c /= Empty && c /= Wall ) m
      entrance = head [ i | (Entrance, i) <- poi ]
      exit = head [ i | (Exit, i) <- poi ]
      transitions = HM.fromList $ [ (idx, dijkstraCover isInteresting isVisitable idx m) | (_, idx) <- poi ]
      levStride = w * h

  qRef <- newIORef $ PSQ.singleton entrance 0 0
  seenRef <- newIORef $ M.empty
  cntRef <- newIORef 0

  let go = do
        q <- readIORef qRef
        modifyIORef' cntRef succ
        cnt <- readIORef cntRef

        -- when (cnt > 10) $ error "stop" -- XXX

        case PSQ.minView q of
          Nothing -> error "Nothing found"
          Just (idxLev, prio, cost, q')
            | idxLev == exit -> do
                print ("COST", cost)
            | otherwise -> do
                writeIORef qRef q'
                let (level, idx) = idxLev `divMod` (w * h)
                visitCell level idx cost
                go

      visitCell level idx cost = do
        modifyIORef' seenRef (M.insert (level * levStride + idx) True)
        seen <- readIORef seenRef

        print ("At", level, cs ! idx, cost, M.size seen)

        let candidates :: [(MapIndex, Cost)] = filter (\(i, _) -> works level (cs ! i)) $ transitions HM.! idx

            works 0 Entrance = True
            works _ Entrance = False
            works 0 Exit = True
            works _ Exit = False
            works 0 (Portal _ _ Outer) = False
            works _ _ = True

            targets = (\(i, thisC) -> (level * levStride + i, cost + thisC)) <$> candidates

            otherSide = case (level, cs ! idx) of
              (_, Entrance) -> []
              (_, Exit) -> []
              (0, Portal _ _ Outer) -> []
              (_, Portal _ (x, y) Inner) -> [ ((level + 1) * levStride + y * w + x, cost + 1) ]
              (_, Portal _ (x, y) Outer) -> [ ((level - 1) * levStride + y * w + x, cost + 1) ]

            validTargets = filter (\(i, _) -> not (M.member i seen)) (otherSide ++ targets)

        print ("VT", validTargets)
        forM_ validTargets $ \(ti, tc) -> do

          let maybeAdd :: Maybe (Int, Cost) -> ((), Maybe (Int, Cost))
              maybeAdd Nothing = ((), Just (ti, tc))
              maybeAdd (Just (_, oldCost)) = ((), Just (ti, min oldCost tc))
          modifyIORef' qRef (snd . PSQ.alter maybeAdd ti)

          pure ()


  go
  pure ()

type Cost = Int
type Level = Int

type SearchState = (Level, Cell, Cost)

shortest :: Width -> Height -> Vector Cell -> IO Int
shortest w h m = do
  let cells = V.toList m
      Just entrance = findIndex (== Entrance) cells
      Just exit = findIndex (== Exit) cells
      startQ = PSQ.singleton entrance 0 ()

  qRef <- newIORef startQ
  seen <- V.thaw $ V.map (const False) m

  let pickNext :: IO (Maybe (Int, Int))
      pickNext = atomicModifyIORef' qRef
       (\q -> case PSQ.minView q of
                Nothing -> (q, Nothing)
                Just (k, p, _, q') -> (q', Just (k, p)))

      renderSeen idx = numLoop 0 (h - 1) $ \y -> do
        numLoop 0 (w - 1) $ \x -> do
          s <- MV.read seen (x + y * w)
          putStr $ if idx == (x + y * w) then "@" else if s then "█" else "."
        putStrLn ""

      isTraversable idx = case m ! idx of
                            Wall -> False
                            _ -> True

      addToQueue cost Nothing = ((), Just (cost, ()))
      addToQueue cost old@(Just (oldCost, v))
        | oldCost <= cost = ((), old)
        | otherwise = ((), Just (cost, ()))

      visitNormal idx cost = do
        MV.write seen idx True
        let adjacent = filter isTraversable [ idx - 1, idx + 1, idx - w, idx + w ]
        unseen <- filterM (\idx -> MV.read seen idx >>= pure . not) adjacent
        atomicModifyIORef qRef $ \q ->
          (foldr (\idx q -> snd (PSQ.alter (addToQueue (cost + 1)) idx q)) q unseen, ())
        -- print $ ("Adjacent", adjacent)
        pure ()

      go = pickNext >>= \case
        Nothing -> pure (-1)
        Just (idx, cost)
          | idx == exit -> pure cost
          | otherwise -> do
              -- putStrLn $ "Visiting " ++ show idx ++ " at " ++ show cost
              -- renderSeen idx
              -- threadDelay 10000
              case m ! idx of
                Empty -> visitNormal idx cost >> go
                Entrance -> visitNormal idx cost >> go
                Exit -> pure cost
                Portal _ (x, y) _ -> do
                  visitNormal idx cost
                  visitNormal (x + y * w) (cost + 1)
                  go
  go

part1 :: IO ()
part1 = do
  (w, h, raw) <- readMap "20.txt"
  let parsed = parseMap w h raw
  -- printMap w h parsed
  -- print $ sort $ [ x | x@(Portal _ _) <- V.toList parsed ]
  shortest w h parsed >>= print
  pure ()

main :: IO ()
main = part2
