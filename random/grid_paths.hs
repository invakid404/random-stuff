import Data.Function.Memoize (memoize2)

solve :: Integer -> Integer -> Integer
solve = (. subtract 1) . solve' . subtract 1
  where
    solve' = memoize2 go
    go x y
      | x == 0 || y == 0 = 1
      | otherwise = solve' (x - 1) y + solve' x (y - 1)

main :: IO ()
main = (getLine >>=) . (print .) . (. read) . solve . read =<< getLine