module Main where

import Data.Char
import Data.List
import IntCode
import Data.IORef
import qualified Data.Vector as V
import Data.Vector (Vector, (!))

data Karnaugh = Karnaugh Int (Vector Bool)

mkKarnaugh :: Int -> Int -> [Bool]
mkKarnaugh count outputs =
  let bits = go count outputs

      go 0 _ = []
      go cnt val = (val `rem` 2 == 1) : go (cnt - 1) (val `div` 2)

  in bits



main :: IO ()
main = do
  code <- readProgram "21.txt"

  -- let test = concat $ intersperse "\n" [ "NOT A J"
  --                                      , "NOT B T"
  --                                      , "AND D T"
  --                                      , "OR T J"
  --                                      , "NOT C T"
  --                                      , "AND D T"
  --                                      , "OR T J"
  --                                      , "WALK\n"
  --                                      ]



  -- .... J
  -- ...# J
  -- ..#. J
  -- ..## J
  -- .#.. J
  -- .#.# J
  -- .##. J
  -- .### J
  -- #... N
  -- #..# J
  -- #.#. N
  -- #.## J
  -- ##.. N
  -- ##.# J
  -- ###. N
  -- #### N

  let test = concat $ intersperse "\n" [ "NOT D T"
                                       , "NOT T T"
                                       , "NOT H J"
                                       , "NOT J J"
                                       , "AND T J"
                                       , "RUN\n"
                                       ]

  -- 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31
  -- 0,1,2,3,4,5,6,7,8,10,11,12,14,15,16,17,18,20,21,22,24,25,26,28,29,30,31
  -- 0,1,2,3,4,5,6,7,8,10,11,12,14
  -- @    EFGHI
  -- ^####.....  0
  -- ^####....#  1
  -- ^####...#.  2
  -- ^####...##  3
  -- ^####..#..  4
  -- ^####..#.#  5
  -- ^####..##.  6
  -- ^####..###  7
  -- ^####.#...  8
  -- ^####.#..#  9 N
  -- ^####.#.#. 10
  -- ^####.#.## 11
  -- ^####.##.. 12
  -- ^####.##.# 13 N
  -- ^####.###. 14
  -- ^####.#### 15 N
  -- ^#####.... 16
  -- ^#####...# 17
  -- ^#####..#. 18
  -- ^#####..## 19 N
  -- ^#####.#.. 20
  -- ^#####.#.# 21
  -- ^#####.##. 22
  -- ^#####.### 23 N
  -- ^######... 24
  -- ^######..# 25
  -- ^######.#. 26
  -- ^######.## 27 N
  -- ^#######.. 28
  -- ^#######.# 29
  -- ^########. 30
  -- ^######### 31


  inputAction <- mkListInput $ (fromIntegral . ord) <$> test
  (outputRef, outputAction) <- mkListOutput
  let opcodes = mkOpcodesTable (pureOpcodes ++ mkIoOpcodes haltAction inputAction outputAction)
      haltAction = pure ()

  vm <- makeRunnable code opcodes
  runIntcode vm runVM

  out <- reverse <$> readIORef outputRef
  putStr $ chr . fromIntegral <$> out
  -- print $ out

  -- print $ mkKarnaugh 16 34293

  pure ()
