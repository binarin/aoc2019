{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Debug.Trace
import GHC.Stack
import Control.Lens
import Control.Lens.TH
import Data.Char
import Data.Array.Unboxed as A

data Ray = Ray { _rayNumberL :: Int
               , _rayLowL :: Int
               , _rayHighL :: Int
               , _rayValueL :: Int
               } deriving (Show)
makeFields ''Ray

basePattern :: [Int]
basePattern = [0, 1, 0, -1]

eltPattern :: Int -> [Int]
eltPattern n = tail $ cycle $ concatMap (replicate n) basePattern

type Width = Int

testInput :: [Int]
testInput = [1,2,3,4,5,6,7,8]

runPhase :: [Int] -> [Int]
runPhase ns = go 1 ns
  where
    go _ [] = []
    go pos (x:xs) = finalize (sum (zipWith (*) (eltPattern pos) ns)) : go (pos + 1) xs

    finalize x = abs x `rem` 10

testInput2 :: [Int]
testInput2 = decode "80871224585914546619083218645595"

testInput3 :: [Int]
testInput3 = decode "19617804207202209144916044189917"

testInput4 :: [Int]
testInput4 = decode "03036732577212944063491565474664"

realInput :: [Int]
realInput = decode "59754835304279095723667830764559994207668723615273907123832849523285892960990393495763064170399328763959561728553125232713663009161639789035331160605704223863754174835946381029543455581717775283582638013183215312822018348826709095340993876483418084566769957325454646682224309983510781204738662326823284208246064957584474684120465225052336374823382738788573365821572559301715471129142028462682986045997614184200503304763967364026464055684787169501819241361777789595715281841253470186857857671012867285957360755646446993278909888646724963166642032217322712337954157163771552371824741783496515778370667935574438315692768492954716331430001072240959235708"

tenThousand :: [Int] -> [Int]
tenThousand = mconcat . replicate 10000

part2Input :: [Int]
part2Input = mconcat $ replicate 10000 realInput

decode :: String -> [Int]
decode = fmap (\c -> ord c - ord '0')

encode :: [Int] -> String
encode = fmap (\i -> chr (i + ord '0'))

-- mkRay :: Int -> Int -> Ray
-- mkRay n v = Ray n (2 * n - 1) (2 * n - 1) v

-- 1: 1,1+     2,3+      3,5+      4,7+
-- 2: 3,3-     6,7-      9,11-     12,15-
-- 3: 5,5+     10,11+    15,17     20,23
-- 4: 7,7-     14,15-    21,23-

type Input = Array Int Int

advanceRay :: Input -> Ray -> Maybe Ray
advanceRay input fr@(Ray rayNo low high value) =
  if low' > upperBound
  then Nothing
  else Just $ Ray rayNo low' high' value'
  where
    (_, upperBound) = bounds input
    stride = rayNo * 2 - 1
    currentGen = low `div` stride
    low' = stride * (currentGen + 1)
    width = high - low
    high' = min (low' + width + 1) upperBound
    discardRange' = [low..min (low'-1) high]
    discardRange = discardRange'
    injectRange = [max (high+1) low'..high']
    removeValue = sum $ (input!) <$> discardRange
    addValue = sum $ (input!) <$> injectRange
    raySign = if rayNo `rem` 2 == 1 then 1 else (-1)
    value' = value - raySign * removeValue + raySign * addValue

projectRay :: Input -> Ray -> [Ray]
projectRay input ray = go ray
  where
    go ray = case advanceRay input ray of
               Nothing -> [ray]
               Just ray' -> ray:go ray'

mkRay :: Input -> Int -> Ray
mkRay input rayNo = Ray rayNo idx idx (raySign * (input ! idx))
  where idx = (2 * rayNo - 1)
        raySign = if rayNo `rem` 2 == 1 then 1 else (-1)

runPhase' :: Input -> Input
runPhase' input =
  let (_, upperBound) = bounds input
      numRays = upperBound `div` 2 + upperBound `rem` 2
      rayProgression = [ projectRay input (mkRay input rayNo) | rayNo <- [1..numRays] ]

      progress :: [[Ray]] -> [[Ray]]
      progress = filter (not . null) . fmap tail

      currentValue :: [[Ray]] -> Int
      currentValue r = abs (sum (view valueL <$> fmap head r)) `rem` 10

      go :: [[Ray]] -> [Int]
      go [] = []
      go rp = currentValue rp : go (progress rp)

  in listArray (1, upperBound) (go rayProgression)

main :: IO ()
main = do
  let input = tenThousand testInput4
      inputLen = length input
      inputA = listArray (1, inputLen) input
      phases = iterate runPhase' inputA

      offset :: Int = read $ encode (take 7 input)

      solution = phases !! 100

  print $ encode $ take 8 $ drop offset $ elems solution
  -- putStrLn $ encode $ take 8 $ A.elems $ phases !! 1

  -- sequence $ print <$> (take 10 $ projectRay $ mkRay 2)
  -- let iterations = iterate runPhase part2Input
  -- print $ encode $ take 8 $ iterations !! 2

  --   1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23
  --   1  0 -1  0  1  0 -1  0  1  0 -1  0
  --  1,1+  3,3- 5,5+  7,7-   9,9+  11,11-

  --   0  1  1  0  0 -1  -1 0  0  1  1  0  0 -1 -1  0  0
  --       2,3+      6,7-        10,11+      14,15-

  --   0  0  1  1  1  0  0  0 -1 -1 -1  0  0  0  1  1  1  0  0  0 -1 -1 -1
  --          3,5+             9,11-               15,17+           21,23-

  --   0  0  0  1  1  1  1  0  0  0  0 -1 -1 -1 -1  0  0  0  0 -1 -1 -1 -1
  --             4,7+                   12,15-                   20,23+

  let sample = 8
      signs = take sample . eltPattern <$> [1..sample]
      visualize s
        | s == 0 = ' '
        | s < 0 = '-'
        | s > 0 = '+'
  sequence $ print <$> (fmap visualize <$> signs)
  pure ()
