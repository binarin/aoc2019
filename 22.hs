{-# LANGUAGE LambdaCase #-}
module Main where

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

intParser :: Parser Int
intParser = do
  negative <- option id (negate <$ char '-')
  numbersString <- read <$> many1 digit
  pure $ negative numbersString

-- f1(f2(x))
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
        -- print $ d'
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


main :: IO ()
main = do
  pure ()
