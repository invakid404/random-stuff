import           Control.Monad   (liftM2)
import           Data.Bits       (Bits (bit, popCount, (.&.), (.|.)))
import           Data.Char       (ord)
import           Data.Foldable   (Foldable (foldl'))
import           Data.List.Split (splitOn)

main :: IO ()
main =
  liftM2
    (>>)
    (print . solve (.|.))
    (print . solve (.&.))
    . map (map (foldl' ((. (bit . subtract (ord 'a') . ord)) . (.|.)) (0 :: Int)))
    . splitOn [""]
    . lines
    =<< readFile "input.txt"
  where
    solve = (sum .) . map . (popCount .) . foldl1
