import Control.Monad (liftM2, replicateM)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V

partOne :: Vector Integer -> Integer
partOne xs
  | c `elem` sums = partOne (V.tail xs)
  | otherwise = c
  where
    c = xs ! 25
    sums = map sum $ replicateM 2 (map (xs !) [0 .. 24])

partTwo :: Vector Integer -> Integer
partTwo = uncurry (+) . liftM2 (,) V.minimum V.maximum . (slice =<< partOne)
  where
    slice target xs = go 0 0 0
      where
        go lo hi acc
          | acc < target = go lo (hi + 1) (acc + xs ! hi)
          | acc > target = go (lo + 1) hi (acc - xs ! lo)
          | otherwise = V.slice lo (hi - lo) xs

main :: IO ()
main =
  liftM2
    (>>)
    (print . partOne)
    (print . partTwo)
    . V.fromList
    . map read
    . lines
    =<< readFile "input.txt"
