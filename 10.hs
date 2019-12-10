module Main where

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.List (sortOn)

import Control.Monad (forM_)
import Data.Array.Unboxed (UArray, listArray)
import Data.Array.IArray ((!))
import Data.Array.ST

type Coord = (Int, Int)
data AsteroidMap = Map Int Int (UArray Int Bool) deriving (Show)

asteroidLocationsInbetween :: Coord -> Coord -> [Coord]
asteroidLocationsInbetween (ax, ay) (bx, by)
  | ax == bx = [ (fromIntegral ax, fromIntegral y) | y <- [min ay by + 1 .. max ay by - 1] ]
  | ay == by = [ (fromIntegral x, fromIntegral ay) | x <- [min ax bx + 1 .. max ax bx - 1] ]
  | abs (ax - bx) <= abs (ay - by) =
    let dx = bx - ax
        dy = by - ay
        ys = if ay < by
             then [ay + 1 .. by - 1]
             else [ay - 1, ay - 2 .. by + 1]
    in [ (ax + (y-ay)*dx `div` dy, y) | y <- ys,  (y-ay)*dx `mod` dy == 0]
  | otherwise =
    (\(y, x) -> (x, y)) <$> asteroidLocationsInbetween (ay, ax) (by, bx)

printMap :: AsteroidMap -> IO ()
printMap (Map width height arr) = do
  forM_ [0..height-1] $ \y -> do
    forM_ [0..width-1] $ \x ->
      putChar (if (arr ! (width * y + x)) then '#' else '.')
    putStrLn ""

parseMap :: ByteString -> AsteroidMap
parseMap str =
  let rows = C8.lines str
      width = B.length $ head rows
      height = length rows

      hasAsteroid '#' = True
      hasAsteroid _ = False

      cells = hasAsteroid <$> C8.unpack (B.concat rows)

      arr = listArray (0, width * height - 1) cells

  in Map width height arr

asteroidLocations :: AsteroidMap -> [Coord]
asteroidLocations (Map width height arr) =
  [ (x, y) | y <- [0..height-1], x <- [0..width-1], arr ! (width * y + x) ]

isVisible :: AsteroidMap -> Coord -> Coord -> Bool
isVisible (Map width height arr) a b =
  let inbetween = asteroidLocationsInbetween a b
      asteroidAt (x, y) = arr ! (width * y + x)
      clearLineOfSight = all (not . asteroidAt) inbetween
  in clearLineOfSight

evaporationRound :: Coord -> AsteroidMap -> ([Coord], AsteroidMap)
evaporationRound location@(lx, ly) plot@(Map width height arr)  =
  let locations = filter (/= location) $ asteroidLocations plot
      visible = filter (isVisible plot location) locations
      angle (x, y) = mapAtan2 (fromIntegral $ x - lx) (fromIntegral $ y - ly)
      evaporationOrder = reverse $ sortOn angle visible
      arr' = runSTUArray $ do
        new <- thaw arr
        forM_ visible $ \(x, y) -> writeArray new (width * y + x) False
        pure new

  in (evaporationOrder, Map width height arr')

mapAtan2 y x = let at = atan2 y x
               in if at >= (-pi/2)
                  then at
                  else at

main :: IO ()
main = do
  rawMap <- B.readFile "10.txt"
  let plot = parseMap rawMap
      locations = asteroidLocations plot
      visibility = [ (coord, length $ filter (isVisible plot coord) (filter (/= coord) locations)) | coord <- locations ]
      bestVisibility = head $ reverse $ sortOn snd visibility
      (evs, plot') = evaporationRound (26, 29) plot

  printMap plot
  putStrLn ""
  printMap plot'
  putStrLn $ show $ take 200 evs
  --  putStrLn $ show evs

  -- putStrLn $ show $ mapAtan2 (-10) 0
  -- putStrLn $ show $ mapAtan2 0 10
  -- putStrLn $ show $ mapAtan2 10 0
  -- putStrLn $ show $ mapAtan2 0 (-10)
  -- putStrLn $ show $ mapAtan2 (-1) (-10)
  -- putStrLn $ show $ mapAtan2 (-10) (-10)
  -- putStrLn $ show $ mapAtan2 (-10) (-1)

  -- putStrLn $ show $ asteroidLocationsInbetween (1,0) (7, 3)
  -- putStrLn $ show $ asteroidLocationsInbetween (1,2) (4, 2)
  -- mapM_ (putStrLn . show) visibility
  putStrLn $ show bestVisibility
  -- putStrLn $ show $ isVisible plot (3,4) (4,0)
  -- putStrLn $ show $ locations
  pure ()
