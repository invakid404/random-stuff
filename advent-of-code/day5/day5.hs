import Control.Monad (liftM2)
import Data.Char (digitToInt)
import Data.Foldable (foldl')
import Data.List.Ordered (minus, sort)

main :: IO ()
main =
  liftM2
    (>>)
    (print . maximum)
    (print . head . (minus =<< enumFrom . head) . sort)
    . map (foldl' ((. digitToInt) . (+) . (2 *)) 0 . (show . fromEnum . (`elem` "RB") =<<))
    . lines
    =<< readFile "input.txt"
