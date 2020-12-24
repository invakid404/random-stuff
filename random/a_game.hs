import Data.Function.Memoize (memoFix2)
import Data.Vector.Unboxed ((!))
import qualified Data.Vector.Unboxed as V

solve :: [Int] -> Int
solve l = solve' 0 (n - 1)
  where
    bottles = V.fromList l
    n = V.length bottles
    solve' =
      memoFix2
        ( \f left right ->
            let year = left + n - right
             in if left > right
                  then 0
                  else
                    max
                      (f (left + 1) right + year * bottles ! left)
                      (f left (right - 1) + year * bottles ! right)
        )

main :: IO ()
main = print . solve . read =<< getLine
