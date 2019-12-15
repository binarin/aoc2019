{-# LANGUAGE LambdaCase #-}
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
mkReactionsMap = M.fromList . fmap toMapElement
  where
    toMapElement (elt, cnt, parts) = (elt, (cnt, parts))

type Wants = Map Element Int

expandElementWants :: ReactionMap -> Element -> Int -> [(Element, Int)]
expandElementWants m elt cnt
  | elt == "ORE" = [(elt, cnt)]
  | otherwise =
      let Just (rOut, parts) = M.lookup elt m
          uneven = cnt `rem` rOut /= 0
          mult = cnt `div` rOut + bool 0 1 uneven
      in over _2 (*mult) <$> parts

-- expandWants :: ReactionMap -> Wants -> Wants
-- expandWants  = foldl (\nm (elt, cnt) -> M.insertWith (+) elt cnt nm) M.empty (toList

expansionRound :: ReactionMap -> Wants -> Wants
expansionRound rm ws =
  let expanded = mconcat $ uncurry (expandElementWants rm) <$> M.toList ws
      addToWants ws (elt, cnt) = M.insertWith (+) elt cnt ws
  in foldl addToWants M.empty expanded


main :: IO ()
main = do
  rs <- readReactions "14-sample3.txt"
  let rmap = mkReactionsMap rs
      expander = expansionRound rmap
      expansions = iterate expander (M.singleton "FUEL" 1)
      solution = takeWhile (\m -> M.size m /= 1) (tail expansions)

  sequence $ print <$> take 10 expansions

  let sec = expansions !! 2
  putStrLn "SEC:"
  sequence $ print <$> M.toList sec
  putStrLn "\nEXP:"
  sequence $ print <$> (uncurry (expandElementWants rmap) <$> M.toList sec)

  -- print $ solution
  -- sequence $ print <$> head solution
  -- print $ expansionRound rmap (M.singleton "AB" 2)
  -- print $ expansionRound rmap (M.singleton "BC" 3)
  -- print $ expansionRound rmap (M.singleton "CA" 4)
  -- print $ expansionRound rmap (M.singleton "A" 10)
  -- print $ expansionRound rmap (M.singleton "B" 23)
  -- print $ expansionRound rmap (M.singleton "C" 37)
  pure ()

-- want 1 FUEL, have nothing
-- want [2 AB, 3 BC, 4 CA]
-- want [6 A, 8 B, 15 B, 21 C, 3 BC, 4 CA] -> [6 A, 23 B, 21 C, 3 BC, 4 CA]
