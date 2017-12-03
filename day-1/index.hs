module Main where

sumIfMatch :: Int -> (Int, Int) -> (Int, Int)
sumIfMatch x (total, y) =
  if x == y
    then (total + x, x)
    else (total, x)

uncaptcha :: [Int] -> Int
uncaptcha (x:xs) =
  fst $ foldr sumIfMatch (0, x) (x:xs)

main = do
  print $ uncaptcha [1, 1, 2, 2] == 3
  print $ uncaptcha [1, 1, 1, 1] == 4
  print $ uncaptcha [9, 1, 2, 1, 2, 1, 2, 9] == 9
