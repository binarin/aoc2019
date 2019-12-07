module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import System.IO
import Control.Exception (bracket, throwIO)

fuelRequirement :: Int -> Int
fuelRequirement m = floor (fromIntegral m / 3) - 2

fuelRequirementRecursive :: Int -> Int
fuelRequirementRecursive m = case (fuelRequirement m) of
                               m' | m' <= 0 -> 0
                                  | otherwise -> m' + fuelRequirementRecursive m'

main :: IO ()
main = bracket (openFile "./01-1.txt" ReadMode) hClose $ \fh -> do
  raw <- B.hGetContents fh
  numbers <- case traverse (fst <$>) (C8.readInt <$> C8.lines raw) of
               Nothing -> error "Failed to parse numbers"
               Just it -> pure it
  putStrLn $ show $ foldr (+) 0 (fuelRequirementRecursive <$> numbers)
  pure ()
