import Control.Monad (liftM2)
import Data.List (sort)
import Data.List.Split (splitPlaces)

readLines :: FilePath -> IO [String]
readLines = (lines `fmap`) . readFile

binaryGo :: Integral t => String -> Char -> Char -> t -> t -> t
binaryGo "" _ _ leftVal _ = leftVal
binaryGo (k : ks) leftKey rightKey leftVal rightVal
  | k == leftKey = binaryGo ks leftKey rightKey leftVal (mid - 1)
  | k == rightKey = binaryGo ks leftKey rightKey (mid + 1) rightVal
  | otherwise = error $ "Unexpected value: " ++ [k]
  where
    mid = leftVal + div (rightVal - leftVal) 2

mex :: (Eq t, Num t) => [t] -> t
mex = subtract 1 . (mex' =<< head)
  where
    mex' curr [] = curr
    mex' curr (l : ls)
      | l /= curr = l
      | otherwise = mex' (curr + 1) ls

parseSeat :: Integral a => [Char] -> a
parseSeat = uncurry ((+) . (8 *)) . parseSeat' . splitPlaces [7, 3]
  where
    parseSeat' [y, x] = (binaryGo y 'F' 'B' 0 127, binaryGo x 'L' 'R' 0 7)

partOne :: [Integer] -> Integer
partOne = maximum

partTwo :: [Integer] -> Integer
partTwo = mex . sort

main :: IO ()
main = liftM2 (>>) (print . partOne) (print . partTwo) . map parseSeat =<< readLines "input.txt"