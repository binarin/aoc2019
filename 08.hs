{-# LANGUAGE OverloadedStrings #-}
module Main where

import Graphics.Gloss
import Data.Tuple.Utils (fst3)
import System.IO
import Control.Exception
import Control.DeepSeq
import Data.Char (ord)
import Data.List (sortOn)

import qualified Data.ByteString as B
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8

type Image = ByteString
type Width = Int
type Height = Int

layerCount :: Width -> Height -> Image -> Int
layerCount w h i = B.length i `div` (w * h)

getLayer :: Width -> Height -> Image -> Int -> ByteString
getLayer w h i l = B.take (w * h) $ B.drop (l * w * h) i

getLayer' = getLayer 25 6

count012 :: ByteString -> (Int, Int, Int)
count012 = B.foldl ctr (0,0,0)
  where
    ctr (c0, c1, c2) 0 = (c0+1, c1, c2)
    ctr (c0, c1, c2) 1 = (c0, c1+1, c2)
    ctr (c0, c1, c2) 2 = (c0, c1, c2+1)
    ctr c _ = c

mergeLayers :: ByteString -> ByteString -> ByteString
mergeLayers front back = B.pack (B.zipWith combinePixels front back)
  where
    combinePixels 2 b = b
    combinePixels a _ = a

render :: Width -> Height -> Image -> Image
render w h i =
  let numLayers = layerCount w h i
      layers = [ getLayer w h i l | l <- [0..numLayers-1 ] ]
  in
      foldl1 mergeLayers layers

showLayer :: Width -> Height -> Image -> IO ()
showLayer w h i =  sequence_ $ drawLine <$> colored
  where
      scanlines = [ B.take w $ B.drop (w * y) i  | y <- [0..h-1] ]

      colorize 0 = "X"
      colorize 1 = " "
      colorize 2 = "_"
      colorize _ = "?"

      drawLine l = C8.putStr l >> putStrLn ""

      colored = B.concatMap colorize <$> scanlines


imageToPicture :: Width -> Height -> Image -> Picture
imageToPicture w h i = bitmapOfByteString w h (BitmapFormat TopToBottom PxRGBA) (B.concatMap colorize i) True
  where
      colorize 0 = "\x00\x00\x00\xff"
      colorize 1 = "\xff\xff\xff\xff"
      colorize 2 = "\xff\x00\xff\xff"
      colorize _ = "\x00\xff\x00\xff"


test :: IO ()
test = pure ()

main :: IO ()
main = do
  rawContent <- B.readFile "08.txt"
  let content = B.map (subtract $ toEnum $ fromEnum  '0') rawContent
      small = B.map (subtract $ toEnum $ fromEnum  '0') "0222112222120000"

  -- showLayer 2 2 (render 2 2 small)


  -- display (InWindow "Nice Window" (200, 200) (10, 10)) white (imageToPicture 2 2 (render 2 2 small))
  display (InWindow "Nice Window" (200, 200) (10, 10)) black (imageToPicture 25 6 (render 25 6 content))
  pure ()
