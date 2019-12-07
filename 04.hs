module Main where

nums = [147981..691423]

adjCount [] = []
adjCount (x:xs) = let (this, rest) = span (==x) xs
                  in 1 + length this : adjCount rest

meetsCriteria :: Int -> Bool
meetsCriteria num = length str == 6 && noDecreasingDigits str && hasStrictAdjDup
  where
    str = show num

    noDecreasingDigits [] = True
    noDecreasingDigits (_:[]) = True
    noDecreasingDigits (a:b:xs)
      | a <= b = noDecreasingDigits (b:xs)
      | otherwise = False

    hasStrictAdjDup = any (==2) $ adjCount str

    hasAdjacentDup [] = False
    hasAdjacentDup (_:[]) = False
    hasAdjacentDup (a:b:xs)
      | a == b = True
      | otherwise = hasAdjacentDup (b:xs)


main :: IO ()
main = do
  putStrLn $ show $ length $ fst $ span (=='1') "112233"
  putStrLn $ show $ adjCount <$> ["112233", "123444", "111122"]
  putStrLn $ show $ length $ filter meetsCriteria nums
