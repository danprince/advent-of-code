module Main where

isTupleEqual :: (Int, Int) -> Bool
isTupleEqual (x, y) = x == y

rotateBy :: Int -> [Int] -> [Int]
rotateBy 0 xs = xs
rotateBy n xs = rotate (n - 1) (last xs : init xs)

findPairs :: [Int] -> [(Int, Int)]
findPairs xs = zip xs ys
  where
    n = length xs `div` 2
    ys = rotateBy n xs

uncaptcha :: [Int] -> Int
uncaptcha xs = sum numbers
  where
    pairs = findPairs xs
    matches = filter isTupleEqual pairs
    numbers = map fst matches

main = do
  print $ (uncaptcha [1, 2, 1, 2]) == 6
  print $ (uncaptcha [1, 2, 2, 1]) == 0
  print $ (uncaptcha [1, 2, 3, 4, 2, 5]) == 4
  print $ (uncaptcha [1, 2, 3, 1, 2, 3]) == 12
  print $ (uncaptcha [1, 2, 1, 3, 1, 4, 1, 5]) == 4
