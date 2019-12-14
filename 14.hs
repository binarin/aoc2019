{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Bool (bool)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Control.Exception
import Data.ByteString
import Text.Parsec.ByteString (Parser, parseFromFile)
import Text.Parsec.Char
import Text.Parsec

type Element = String

data ParsingException = ParsingException String deriving (Show)
instance Exception ParsingException

intParser :: Parser Int
intParser = do
  negative <- option id (negate <$ char '-')
  numbersString <- read <$> many1 digit
  pure $ negative numbersString

elementWithCountParser :: Parser (Element, Int)
elementWithCountParser = do
  count <- intParser
  char ' '
  element <- many1 letter
  pure $ (element, count)

reactionParser :: Parser (Element, Int, [(Element, Int)])
reactionParser = do
  sources <- elementWithCountParser `sepBy` (string ", ")
  string " => "
  (elt, cnt) <- elementWithCountParser
  pure (elt, cnt, sources)

reactionFileParser :: Parser [(Element, Int, [(Element, Int)])]
reactionFileParser = endBy reactionParser newline

readReactions :: FilePath -> IO [(Element, Int, [(Element, Int)])]
readReactions path = parseFromFile reactionFileParser path >>= \case
  Left error -> throwIO $ ParsingException $ show error
  Right parsed -> pure parsed


type ReactionMap = Map Element (Int, [(Element, Int)])

mkReactionsMap :: [(Element, Int, [(Element, Int)])] -> ReactionMap
mkReactionsMap elts = undefined

type Wants = Map Element Int

expandElementWants :: ReactionMap -> Element -> Int -> [(Element, Int)]
expandElementWants m elt cnt =
  let Just (rOut, parts) = M.lookup elt m
      uneven = cnt `rem` rOut /= 0
      mult = cnt `div` rOut + bool 0 1 uneven
  in undefined

-- expandWants :: ReactionMap -> Wants -> Wants
-- expandWants  = foldl (\nm (elt, cnt) -> M.insertWith (+) elt cnt nm) M.empty (toList




main :: IO ()
main = do
  readReactions "14.txt" >>= print
  pure ()

-- want 1 FUEL, have nothing
-- want [2 AB, 3 BC, 4 CA]
-- want [6 A, 8 B, 15 B, 21 C, 3 BC, 4 CA] -> [6 A, 23 B, 21 C, 3 BC, 4 CA]
