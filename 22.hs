{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.Extra (whileM)
import Data.STRef
import Control.Loop
import Data.IORef
import Data.List
import Control.Exception (throwIO, Exception)
import Control.Monad.ST
import qualified Data.Vector as V
import Data.Vector (Vector, (!))
import qualified Data.Vector.Mutable as MV

import Text.Parsec.ByteString (Parser, parseFromFile)
import Text.Parsec.Char
import Text.Parsec

data Op = DealNew | DealInc Integer | Cut Integer deriving (Show)

data LF = LF Integer Integer Integer deriving (Show)

opToLf :: Integer -> Op -> LF
opToLf mod DealNew = LF (-1) (-1) mod
opToLf mod (Cut c) = LF 1 (-c) mod
opToLf mod (DealInc i) = LF i 0 mod

applyLf :: LF -> Integer -> Integer
applyLf (LF a b m) x = (a * x + b) `mod` m

expandLf :: LF -> [Integer]
expandLf lf@(LF _ _ m) = runST $ do
  v <- MV.new $ fromIntegral m
  numLoop 0 (m-1) $ \x -> do
    MV.write v (fromIntegral $ applyLf lf x) x
  V.toList <$> V.freeze v

intParser :: Parser Int
intParser = do
  negative <- option id (negate <$ char '-')
  numbersString <- read <$> many1 digit
  pure $ negative numbersString

-- f1 `compose` f2 = f1(f2(x))
compose :: LF -> LF -> LF
compose (LF a1 b1 m) (LF a2 b2 _) =
  LF ((a1 * a2) `mod` m) ((a1 * b2 + b1) `mod` m) m

dealNewParser :: Parser Op
dealNewParser = string "deal into new stack" >> pure DealNew

dealIncParser :: Parser Op
dealIncParser = do
  string "deal with increment "
  inc <- intParser
  pure $ DealInc $ fromIntegral inc

cutParser :: Parser Op
cutParser = do
  string "cut "
  num <- intParser
  pure $ Cut $ fromIntegral num

shuffleLineParser :: Parser Op
shuffleLineParser = try cutParser <|> try dealIncParser <|> try dealNewParser

shuffleFileParser :: Parser [Op]
shuffleFileParser = endBy shuffleLineParser newline

data ParsingException = ParsingException String deriving (Show)
instance Exception ParsingException

applyOp :: Op -> [Integer] -> [Integer]
applyOp DealNew = dealNew
applyOp (DealInc i) = dealInc i
applyOp (Cut i) = cut i

deck :: Int -> [Integer]
deck cnt = take cnt $ iterate succ 0

dealNew :: [Integer] -> [Integer]
dealNew = reverse

cut :: Integer -> [Integer] -> [Integer]
cut n s
  | n == 0 = s
  | n > 0 = drop (fromIntegral n) s ++ take (fromIntegral n) s
  | otherwise = cut (fromIntegral $ length s + fromIntegral n) s

dealInc :: Integer -> [Integer] -> [Integer]
dealInc n s = runST $ do
  let v = V.fromList s
      l = V.length v

  out <- MV.new l

  let go c
       | c >= l = pure ()
       | otherwise = do
           MV.write out (fromIntegral ((fromIntegral c * n) `rem` fromIntegral l)) (v ! c)
           go (c + 1)

  go 0

  V.toList <$> V.freeze out

part1 :: IO ()
part1 = do
  let go [] d = pure d
      go (op:ops) d = do
        let d' = applyOp op d
        print $ d'
        go ops d'

  ops <- parseFromFile shuffleFileParser "22.txt" >>= \case
    Left error -> throwIO $ ParsingException $ show error
    Right parsed -> pure parsed


  -- print $ dealInc 3 $ deck 10
  last <- go ops (deck 10007)
  print $ findIndex (==2019) last

  putStrLn ""
  print $ dealNew $ cut 2 $ deck 10
  print $ dealNew $ cut 8 $ dealInc 7 $ deck 10
  print $ dealNew $ cut 0 $ dealInc 7 $ deck 10
  print $ dealNew $ cut 4 $ dealInc 7 $ deck 10
  putStrLn ""
  print $ dealInc 7 $ cut 4 $ deck 10
  print $ dealInc 9 $ cut 4 $ deck 10
  pure ()


part1' :: IO ()
part1' = do
  ops <- parseFromFile shuffleFileParser "22.txt" >>= \case
    Left error -> throwIO $ ParsingException $ show error
    Right parsed -> pure parsed

  let ct = 10007
      startLf = LF 1 0 ct

  let go _ [] d _ = pure d
      go 0 _ d _ = pure d
      go lim (op:ops) d lf = do
        let d' = applyOp op d
            oplf = opToLf ct op
            lf' = oplf `compose` lf
        print op
        print $ (oplf, lf, lf')
        print $ take 10 $ d'
        print $ take 10 $ expandLf lf'
        print $ [ applyLf (inverseLF lf') x | x <- [0..9]]
        putStrLn ""
        go (lim - 1) ops d' lf'

  go 5 ops [0..ct-1] startLf
  let lf = foldr (flip compose) (LF 1 0 ct) (opToLf ct <$> ops)
  print lf
  print $ applyLf lf 2019
  pure ()

lfPow :: LF -> Integer -> LF
lfPow (LF a b m) 0 = LF 1 0 m
lfPow lf         1 = lf
lfPow lf         n =
  let (halfN, rem) = n `divMod` 2
      lf2 = lf `compose` lf
  in case rem of
    0 -> lfPow lf2 halfN
    1 -> lf `compose` lfPow lf2 halfN



part2 :: IO ()
part2 = do
  ops <- parseFromFile shuffleFileParser "22.txt" >>= \case
    Left error -> throwIO $ ParsingException $ show error
    Right parsed -> pure parsed

  let deckSize = 119315717514047
      iterations = 101741582076661
      lf = foldr (flip compose) (LF 1 0 deckSize) (opToLf deckSize <$> ops)
      lf' = lfPow lf iterations
  print $ applyLf (inverseLF lf') 2020
  pure ()

inverse :: Integer -> Integer -> Integer
inverse a n = runST $ do
  t <- newSTRef 0
  newT <- newSTRef 1
  r <- newSTRef n
  newR <- newSTRef a
  whileM $ do
    quot <- div <$> readSTRef r <*> readSTRef newR
    (,) <$> readSTRef t <*> readSTRef newT >>= \(tV, newTV) -> do
      writeSTRef t newTV
      writeSTRef newT (tV - quot * newTV)
    (,) <$> readSTRef r <*> readSTRef newR >>= \(rV, newRV) -> do
      writeSTRef r newRV
      writeSTRef newR (rV - quot * newRV)
    (/= 0) <$> readSTRef newR
  readSTRef t >>= \case
    tv
      | tv < 0 -> pure $ tv + n
      | otherwise -> pure $ tv

inverseLF :: LF -> LF
inverseLF (LF a b m) =
  let aInv = inverse a m
  in LF aInv ((-b * aInv) `mod` m) m


main :: IO ()
main = do
  -- part1'
  -- print $ (3409 * (inverse 3409 10007)) `mod` 10007
  part2

  -- print $ LF 65 0 10007 `compose` LF (-1) (-1) 10007

part1Tests :: IO ()
part1Tests = do
  ops <- parseFromFile shuffleFileParser "22.txt" >>= \case
    Left error -> throwIO $ ParsingException $ show error
    Right parsed -> pure parsed

  let ct = 10

  let go [] d = pure d
      go (op:ops) d = do
        let d' = applyOp op d
        print $ d'
        go ops d'

  let prim1 = opToLf ct (Cut 6)
      prim2 = opToLf ct (DealInc 7)
      prim3 = opToLf ct (DealNew)

      s1 = LF 1 0 ct `compose` prim1
      s2 = s1 `compose` prim2
      s3 = s2 `compose` prim3

      ss = foldr compose (LF 1 0 ct) [prim1, prim2, prim3]

  go [Cut 6, DealInc 7, DealNew] [0..ct - 1]

  putStrLn ""
  print $ expandLf prim1
  print $ expandLf prim2
  print $ expandLf prim3

  putStrLn ""

  print $ expandLf $ s1
  print $ expandLf $ s2
  print $ expandLf $ s3

  putStrLn ""
  print $ expandLf ss

  pure ()
