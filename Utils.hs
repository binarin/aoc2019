module Utils where

takeWhileUnique :: Eq a => [a] -> [a]
takeWhileUnique [] = []
takeWhileUnique [a] = [a]
takeWhileUnique (a:b:rest)
  | a == b = [a]
  | otherwise = a : takeWhileUnique (b:rest)

dropWhileUnique :: Eq a => [a] -> [a]
dropWhileUnique [] = []
dropWhileUnique [a] = [a]
dropWhileUnique (a:b:rest)
  | a == b = (b:rest)
  | otherwise = dropWhileUnique (b:rest)
