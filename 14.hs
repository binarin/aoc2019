{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
module Main where

import Control.Lens
import Data.Bool (bool)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Control.Exception
import qualified Data.ByteString as B
import Text.Parsec.ByteString (Parser, parseFromFile)
import Text.Parsec.Char
import Text.Parsec

data ParsingException = ParsingException String deriving (Show)
instance Exception ParsingException

intParser :: Parser Int
intParser = do
  negative <- option id (negate <$ char '-')
  numbersString <- read <$> many1 digit
  pure $ negative numbersString

elementWithCountParser :: Parser (Element, Count)
elementWithCountParser = do
  count <- intParser
  char ' '
  element <- many1 letter
  pure $ (element, count)

reactionParser :: Parser (Element, Count, [(Element, Count)])
reactionParser = do
  sources <- elementWithCountParser `sepBy` (string ", ")
  string " => "
  (elt, cnt) <- elementWithCountParser
  pure (elt, cnt, sources)

reactionFileParser :: Parser [(Element, Count, [(Element, Count)])]
reactionFileParser = endBy reactionParser newline

readReactions :: FilePath -> IO [(Element, Count, [(Element, Count)])]
readReactions path = parseFromFile reactionFileParser path >>= \case
  Left error -> throwIO $ ParsingException $ show error
  Right parsed -> pure parsed

type Count = Int
type ReactionMap = Map Element (Count, [(Element, Count)])
type Element = String
type Reactor = Map Element Count

mkReactionsMap :: [(Element, Count, [(Element, Count)])] -> ReactionMap
mkReactionsMap = M.fromList . fmap toMapElement
  where
    toMapElement (elt, cnt, parts) = (elt, (cnt, parts))

divUp :: Integral a => a -> a -> a
divUp a b
  | a `rem` b == 0 = a `div` b
  | otherwise = a `div` b + 1


react :: ReactionMap -> Reactor -> Reactor
react rm r = if null negativeNonOre then r else r'
  where
    negativeNonOre = filter (\(e, c) -> c < 0 && e /= "ORE") (M.toList r)
    (firstNegativeElt, negativeCount) = head negativeNonOre
    missingCount = - negativeCount
    r' =
      let Just (outCnt, inputs) = M.lookup firstNegativeElt rm
          mult = missingCount `divUp` outCnt
          negativeInuputsMultiples = (\(elt, cnt) -> (elt, - cnt * mult )) <$> inputs
          updates = (firstNegativeElt, outCnt * mult):negativeInuputsMultiples
      in foldr (\(elt, cnt) r -> M.insertWith (+) elt cnt r) r updates

takeWhileUnique :: Eq a => [a] -> [a]
takeWhileUnique [] = []
takeWhileUnique [a] = [a]
takeWhileUnique (a:b:rest)
  | a == b = [a]
  | otherwise = a : takeWhileUnique (b:rest)

oreForFuel :: ReactionMap -> Count -> Count
oreForFuel rmap cnt =
  let reactor = M.singleton "FUEL" (-cnt)
      steps = iterate (react rmap) reactor
      Just solution = M.lookup "ORE" (last $ takeWhileUnique steps)
  in -solution

hasOre :: Count
hasOre = 1_000_000_000_000

binarySearch :: ReactionMap -> Count -> Count -> Count
binarySearch rmap low high
  | low + 1 >= high = low
  | otherwise =
    let middle = low + (high - low) `div` 2
        middleSolution = oreForFuel rmap middle
    in case middleSolution of
      _
       | middleSolution == hasOre -> middle
       | middleSolution < hasOre -> binarySearch rmap middle high
       | otherwise -> binarySearch rmap low middle

main :: IO ()
main = do
  rs <- readReactions "14.txt"
  let rmap = mkReactionsMap rs
      upperBound = 10_000_000
      solution = oreForFuel rmap upperBound

  -- sequence $ print <$> takeWhileUnique steps
  print $ binarySearch rmap 1 upperBound
  pure ()
-- want 1 FUEL, have nothing
-- want [2 AB, 3 BC, 4 CA]
-- want [6 A, 8 B, 15 B, 21 C, 3 BC, 4 CA] -> [6 A, 23 B, 21 C, 3 BC, 4 CA]
