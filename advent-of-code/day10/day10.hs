import Control.Monad (ap, liftM2)
import Data.Function.Memoize (memoFix)
import Data.List (foldl', sort)
import qualified Data.MultiSet as MS

partOne :: [Integer] -> MS.Occur
partOne = foldl' ((. snd) . (*)) 1 . MS.toOccurList . MS.fromList . (zipWith (-) =<< tail)

partTwo :: [Integer] -> Integer
partTwo list = partTwo' 0
  where
    partTwo' =
      memoFix
        ( \f num ->
            if num == last list
              then 1
              else sum (map f (filter (`elem` list) [num + 1 .. num + 3]))
        )

main :: IO ()
main =
  liftM2
    (>>)
    (print . partOne)
    (print . partTwo)
    . ((0 :) . ap (++) (return . (3 +) . last))
    . sort
    . map read
    . lines
    =<< readFile "input.txt"
