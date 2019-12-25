module Main where

import Data.List
import IntCode
import Data.Char
import Control.Monad.IO.Class


main :: IO ()
main = do
  code <- readProgram "25.txt"

  let initialInput = [ "west"
                     , "take hologram"
                     , "north"
                     , "take space heater"
                     , "east"
                     , "take space law space brochure"
                     , "east"
                     , "take tambourine"
                     , "west"
                     , "west"
                     , "south"
                     , "east"
                     , "east"
                     , "take festive hat"
                     , "east"
                     , "take food ration"
                     , "east"
                     , "take spool of cat6"
                     , "west"
                     , "west"
                     , "south"
                     , "east"
                     , "east"
                     , "take fuel cell"
                     , "east"
                     , "inv"
                     , "south"
                     ]

      items = [ "fuel cell"
              , "space heater"
              , "hologram"
              , "space law space brochure"
              , "food ration"
              , "tambourine"
              , "spool of cat6"
              , "festive hat"
              ]

  let generateAttempt [] _ = ["south"]
      generateAttempt (i:is) num =
        let action = if num `rem` 2 == 1 then "take" else "drop"
        in (action ++ " " ++ i) : generateAttempt is (num `div` 2)

      input = initialInput ++ concat [ generateAttempt items i | i <- [0..255] ]

  (outputRef, outputAction) <- mkListOutput

  predefinedInputAction <- mkListInput $ (fromIntegral . ord) <$> intercalate "\n" input

  let haltAction = pure ()
      -- inputAction = liftIO $ (fromIntegral . ord) <$> getChar
      outputAction w = liftIO $ putChar $ chr $ fromIntegral w
      opcodes = mkOpcodesTable $ pureOpcodes ++ mkIoOpcodes haltAction predefinedInputAction outputAction

  vm <- makeRunnable code opcodes

  runIntcode vm runVM

  pure ()
