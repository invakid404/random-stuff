import Data.Function.Memoize (memoFix3)
import Data.Vector ((!))
import qualified Data.Vector as V

solve :: [Integer] -> Integer
solve l = solve' 1 0 (length l - 1)
  where
    bottles = V.fromList l
    solve' =
      memoFix3
        ( \f year left right ->
            if left > right
              then 0
              else
                max
                  (f (year + 1) (left + 1) right + year * bottles ! left)
                  (f (year + 1) left (right - 1) + year * bottles ! right)
        )

main :: IO ()
main = print . solve . read =<< getLine