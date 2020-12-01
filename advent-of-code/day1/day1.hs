import Control.Monad (replicateM)

readLines :: FilePath -> IO [String]
readLines = (lines `fmap`) . readFile

solve :: Int -> [Int] -> Int
solve = ((product . head . filter ((2020 ==) . sum)) .) . replicateM

main :: IO ()
main = do
  lines <- readLines "input.txt"
  let nums = map read lines
  print (solve 2 nums) -- Part 1
  print (solve 3 nums) -- Part 2
