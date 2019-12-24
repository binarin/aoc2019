module Main where

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
  m <- readMap2D False parseCell "24.txt"
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
