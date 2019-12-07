{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Maybe (isJust, fromJust)
import qualified Data.HashSet as HS
import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import System.IO
import Control.Exception (bracket, throwIO)

type OrbitName = ByteString

parseMapLines :: ByteString -> [(OrbitName, OrbitName)]
parseMapLines raw = split <$> C8.lines raw
  where
    split = unpack . C8.split ')'

    unpack (center:what:_) = (center, what)
    unpack it = error $ show it

type Map = HM.HashMap OrbitName [OrbitName]
type Seen = HS.HashSet OrbitName

parseMap :: ByteString -> Map
parseMap = foldr insertBoth mempty . parseMapLines
  where
    insertBoth (center, what) m = HM.insertWith (<>) center [what] (HM.insertWith (<>) what [center] m)

orbitCounter :: Map -> Seen -> Int -> OrbitName -> Int
orbitCounter m seen currentDepth nm = foldr (+) currentDepth orbitsDepths
  where
    orbits :: [OrbitName]
    orbits = maybe [] id $ HM.lookup nm m

    unseenOrbits = filter (not . flip HS.member seen) orbits

    seen' = HS.insert nm seen

    orbitsDepths :: [Int]
    orbitsDepths = orbitCounter m seen' (currentDepth + 1) <$> unseenOrbits

pathLength :: Map -> Seen -> Int -> OrbitName -> OrbitName -> Maybe Int
pathLength map seen currentLen targetOrbit currentOrbit
  | currentOrbit == targetOrbit = Just currentLen
  | otherwise = case validSubSolutions of
      [] -> Nothing
      (x:_) -> Just x

  where
    orbits :: [OrbitName]
    orbits = maybe [] id $ HM.lookup currentOrbit map

    unseenOrbits = filter (not . flip HS.member seen) orbits

    seen' = HS.insert currentOrbit seen

    allSubSolutions = pathLength map seen' (currentLen + 1) targetOrbit <$> unseenOrbits
    validSubSolutions = fromJust <$> filter isJust allSubSolutions

main :: IO ()
main = do
  map <- parseMap <$> bracket (openFile "06.txt" ReadMode) hClose B.hGetContents
  putStrLn $ show $ orbitCounter map mempty 0 "COM"
  putStrLn $ show $ pathLength map mempty 0 "YOU" "SAN"
