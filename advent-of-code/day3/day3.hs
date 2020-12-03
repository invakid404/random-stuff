import Control.Monad

readLines :: FilePath -> IO [String]
readLines = (lines `fmap`) . readFile

solve :: [(Int, Int)] -> [String] -> [Int]
solve deltas field = map (uncurry (solve' field 0 0)) deltas
  where
    solve' [] acc _ _ _ = acc
    solve' (f : fs) acc y dy dx = solve' (drop (dx - 1) fs) (acc + fromEnum ((f !! y) == '#')) ((y + dy) `mod` length f) dy dx

main :: IO ()
main = liftM2 (>>) print (print . product) . solve [(3, 1), (1, 1), (5, 1), (7, 1), (1, 2)] =<< readLines "input.txt"