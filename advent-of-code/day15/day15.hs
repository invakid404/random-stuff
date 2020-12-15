import Control.Monad (foldM, forM_, liftM2)
import Control.Monad.ST (ST, runST)
import Data.Array.ST
  ( MArray (newArray),
    STUArray,
    readArray,
    writeArray,
  )
import Data.List.Split (splitOn)

if' :: Bool -> p -> p -> p
if' True x _ = x
if' False _ y = y

solve :: Int -> [Int] -> Int
solve n inp = runST $ do
  arr <- newArray (0, n) (-1) :: ST s (STUArray s Int Int)
  forM_ (zip inp [1 ..]) $ uncurry (writeArray arr)

  flip (`foldM` 0) [length inp + 1 .. n - 1] $ \v i -> do
    val <- readArray arr v
    writeArray arr v i

    pure $ if' (val == -1) 0 (i - val)

main :: IO ()
main =
  liftM2
    (>>)
    (print . solve 2020)
    (print . solve 30000000)
    . map read
    . splitOn ","
    =<< readFile "input.txt"