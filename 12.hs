{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Debug.Trace
import Data.List
import Control.Lens
import Control.Loop
import Criterion.Main
import Data.Array.IO
import Data.Array.Unboxed
import Data.Array.Unsafe
import Data.Array.MArray
import Control.Monad
import Control.Exception
import Text.Parsec.ByteString (Parser, parseFromFile)
import Text.Parsec.Char
import Text.Parsec

type Vector = (Int, Int, Int)
data Moon = Moon Vector Vector

instance Show Moon where
  show (Moon c v) = "pos=" ++ sv c ++ ", vel=" ++ sv v
    where
      sv :: (Int, Int, Int) -> String
      sv (x, y, z) = "<x=" ++ align x ++ ", y=" ++ align y ++ ", z=" ++ align z ++ ">"

      align :: Show a => a -> String
      align s
        | length (show s) < 4 = replicate (4 - length (show s)) ' ' ++ show s
        | otherwise = show s


intParser :: Parser Int
intParser = do
  negative <- option id (negate <$ char '-')
  numbersString <- read <$> many1 digit
  pure $ negative numbersString

vectorParser :: Parser Vector
vectorParser = do
  string "<x="
  x <- intParser
  string ", y="
  y <- intParser
  pure $ (x,y,0)
  string ", z="
  z <- intParser
  string ">\n"
  pure $ (x, y, z)

data ParsingException = ParsingException String deriving (Show)
instance Exception ParsingException

applyGravityOneAxis :: Int -> Int -> Int -> Int
applyGravityOneAxis thisC otherC thisV
  | thisC == otherC = thisV
  | thisC < otherC = thisV + 1
  | otherwise = thisV - 1

applyGravity :: Moon -> Moon -> Moon
applyGravity (Moon tc@(tx, ty, tz) (tvx, tvy, tvz)) (Moon (ox, oy, oz) _) =
  Moon tc (applyGravityOneAxis tx ox tvx, applyGravityOneAxis ty oy tvy, applyGravityOneAxis tz oz tvz)


type MoonAxis = (Int, Int)

applyGravityMoonAxis :: MoonAxis -> MoonAxis -> MoonAxis
applyGravityMoonAxis t@(tc, tv) (oc, ov)
  | tc == oc = t
  | tc < oc = (tc, tv + 1)
  | otherwise = (tc, tv - 1)

applyAxisCrossGravity :: [MoonAxis] -> [MoonAxis]
applyAxisCrossGravity axes = go [] axes
  where
    go _ [] = []
    go pre (x:xs) = foldl applyGravityMoonAxis x (pre ++ xs) : go (x:pre) xs

splitMoonsIntoAxes :: [Moon] -> [(MoonAxis, MoonAxis, MoonAxis)]
splitMoonsIntoAxes moons = split <$> moons
  where
    split :: Moon -> (MoonAxis, MoonAxis, MoonAxis)
    split (Moon (x, y, z) (dx, dy, dz)) = ((x, dx), (y, dy), (z, dz))

applyAxisSpeed :: [MoonAxis] -> [MoonAxis]
applyAxisSpeed = fmap bump
  where
    bump (c, dc) = (c + dc, dc)

axisSimulationStep :: [MoonAxis] -> [MoonAxis]
axisSimulationStep = applyAxisSpeed . applyAxisCrossGravity



-- applyCrossGravity :: [Moon] -> [Moon]
-- applyCrossGravity moons = go [] moons
--   where
--     go _ [] = []
--     go pre (x:xs) = foldl applyGravity x (pre ++ xs) : go (x:pre) xs


applyCrossGravity :: [Moon] -> [Moon]
applyCrossGravity [m1, m2, m3, m4] = [foldl applyGravity m1 [m2, m3, m4]
                                     ,foldl applyGravity m2 [m1, m3, m4]
                                     ,foldl applyGravity m3 [m2, m1, m4]
                                     ,foldl applyGravity m4 [m2, m3, m1]
                                     ]

applySpeed :: Moon -> Moon
applySpeed (Moon (x, y, z) d@(dx, dy, dz)) = Moon (x + dx, y + dy, z + dz) d

simulationStep :: [Moon] -> [Moon]
simulationStep moons = applySpeed <$> applyCrossGravity moons

potentialEnergy :: Moon -> Int
potentialEnergy (Moon (x, y, z) _) = abs x + abs y + abs z

kineticEnergy :: Moon -> Int
kineticEnergy (Moon _ (dx, dy, dz)) = abs dx + abs dy + abs dz

fullEnergy :: Moon -> Int
fullEnergy m = potentialEnergy m * kineticEnergy m

type MoonsAsArray = IOUArray Int Int

mkMoonsArray :: [Moon] -> IO (MoonsAsArray, Int)
mkMoonsArray moons = do
  let count = length moons
      idx = (0, count * 6 - 1)
  arr <- newListArray idx (concatMap expandMoon moons)
  pure (arr, count)
  where
    expandMoon (Moon (x, y, z) (dx, dy, dz)) = [x, y, z, dx, dy, dz]

applyGravityArray :: MoonsAsArray -> Int -> Int -> IO ()
applyGravityArray moons moonCnt moonNo = do
  let itx = moonNo * 6
      ity = itx + 1
      itz = itx + 2
      itdx = itx + 3
      itdy = itx + 4
      itdz = itx + 5

  numLoop 0 (moonCnt - 1) $ \io -> when (io /= moonNo) $ do
    tx <- readArray moons itx
    ty <- readArray moons ity
    tz <- readArray moons itz

    let iox = io * 6
        ioy = iox + 1
        ioz = iox + 2

    ox <- readArray moons iox
    oy <- readArray moons ioy
    oz <- readArray moons ioz

    tdx <- readArray moons itdx
    tdy <- readArray moons itdy
    tdz <- readArray moons itdz

    when (tx /= ox) $ writeArray moons itdx (if tx < ox then tdx + 1 else tdx - 1)
    when (ty /= oy) $ writeArray moons itdy (if ty < oy then tdy + 1 else tdy - 1)
    when (tz /= oz) $ writeArray moons itdz (if tz < oz then tdz + 1 else tdz - 1)


applySpeedArray :: MoonsAsArray -> Int -> IO ()
applySpeedArray ms mCnt = do
  numLoop 0 (mCnt - 1) $ \i -> do
    let ix = i * 6
        iy = ix + 1
        iz = ix + 2
        idx = ix + 3
        idy = ix + 4
        idz = ix + 5
    dx <- readArray ms idx
    x <- readArray ms ix
    writeArray ms ix (x + dx)

    dy <- readArray ms idy
    y <- readArray ms iy
    writeArray ms iy (y + dy)

    dz <- readArray ms idz
    z <- readArray ms iz
    writeArray ms iz (z + dz)


-- applyGravity

stepArray :: MoonsAsArray -> Int -> IO ()
stepArray ms mCnt = do
  numLoop 0 (mCnt - 1) (applyGravityArray ms mCnt)
  applySpeedArray ms mCnt


lcm' :: Integral a => a -> a -> a
lcm' a b = a * b `div` gcd' a b

gcd' :: Integral a => a -> a -> a
gcd' 0 b = b
gcd' a 0 = a
gcd' a b
  | a < b = gcd' b a
  | otherwise = let q = a `div` b
                    r = a `rem` b
                    -- a = b * q + r
                in gcd' b r

main :: IO ()
main = do
  moons <- parseFromFile (many vectorParser) "12.txt" >>= \case
    Left error -> throwIO $ ParsingException $ show error
    Right parsed -> pure $ flip Moon (0,0,0) <$> parsed

  let xAxes = view _1 <$> splitMoonsIntoAxes moons
      yAxes = view _2 <$> splitMoonsIntoAxes moons
      zAxes = view _3 <$> splitMoonsIntoAxes moons

      xSimulation = iterate axisSimulationStep xAxes
      ySimulation = iterate axisSimulationStep yAxes
      zSimulation = iterate axisSimulationStep zAxes

      go initial simulation ctr
        | head simulation == initial = ctr
        | otherwise = go initial (tail simulation) (ctr + 1)

      xLoop = go xAxes (tail xSimulation) 1
      yLoop = go yAxes (tail ySimulation) 1
      zLoop = go zAxes (tail zSimulation) 1

      commonLoop :: Integer = lcm' (lcm' (fromIntegral zLoop) (fromIntegral yLoop)) (fromIntegral xLoop)

  print xLoop
  print yLoop
  print zLoop
  print commonLoop

  -- (moonsA, moonCnt) <- mkMoonsArray moons
  -- original :: UArray Int Int <- freeze moonsA

  -- let go stepNo = do
  --       stepArray moonsA moonCnt
  --       frozen <- unsafeFreeze moonsA
  --       case frozen == original of
  --         True -> pure stepNo
  --         False -> go (stepNo + 1)

  -- numLoop 1 2772 $ \_ -> stepArray moonsA moonCnt
  -- go 1 >>= print

  -- l <- getElems moonsA
  -- print l

  -- sequence $ print <$> moons
  -- putStrLn ""
  -- sequence $ print <$> simulationStep moons
  -- putStrLn ""
  -- let simulation = iterate simulationStep moons

  -- -- defaultMain [bench "100_000" (whnf (simulation !!) 20000)]

  -- print $ foldr (\m a -> fullEnergy m + a) 0 (simulation !! 100000)

  pure ()
