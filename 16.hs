module Main where

import Data.Char
import Data.Array.IO

basePattern :: [Int]
basePattern = [0, 1, 0, -1]

eltPattern :: Int -> [Int]
eltPattern n = tail $ cycle $ concatMap (replicate n) basePattern

type Width = Int
type RayNo = Int
type Ray = (RayNo, Bool, Int, Int)

adjustRays :: Int -> [Ray] -> (Ray, [Int], [Int])
adjustRays width rays = go rays
  where



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

realInput :: [Int]
realInput = decode "59754835304279095723667830764559994207668723615273907123832849523285892960990393495763064170399328763959561728553125232713663009161639789035331160605704223863754174835946381029543455581717775283582638013183215312822018348826709095340993876483418084566769957325454646682224309983510781204738662326823284208246064957584474684120465225052336374823382738788573365821572559301715471129142028462682986045997614184200503304763967364026464055684787169501819241361777789595715281841253470186857857671012867285957360755646446993278909888646724963166642032217322712337954157163771552371824741783496515778370667935574438315692768492954716331430001072240959235708"

part2Input :: [Int]
part2Input = mconcat $ replicate 10000 realInput

decode :: String -> [Int]
decode = fmap (\c -> ord c - ord '0')

encode :: [Int] -> String
encode = fmap (\i -> chr (i + ord '0'))






main :: IO ()
main = do
  -- let iterations = iterate runPhase part2Input
  -- print $ encode $ take 8 $ iterations !! 2
  let sample = 30
      signs = take sample . eltPattern <$> [1..sample]
      visualize s
        | s == 0 = ' '
        | s < 0 = '-'
        | s > 0 = '+'
  sequence $ print <$> (fmap visualize <$> signs)
  pure ()
