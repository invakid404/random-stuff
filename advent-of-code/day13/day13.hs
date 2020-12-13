import Control.Monad (ap, join, liftM2, zipWithM)
import Data.Either (fromRight)
import Data.List.Split (splitOn)
import Text.Read (readMaybe)

parseInput :: [String] -> (Integer, [(Integer, Integer)])
parseInput [timestamp, buses] = (read timestamp, [(i, b) | (i, Just b) <- zip [0 ..] (map readMaybe $ splitOn "," buses)])

egcd :: Integral b => b -> b -> (b, b)
egcd _ 0 = (1, 0)
egcd a b = (t, s - q * t)
  where
    (s, t) = egcd b r
    (q, r) = a `quotRem` b

modInv :: (Integral a, Show a) => a -> a -> Either [Char] a
modInv a b =
  case egcd a b of
    (x, y)
      | a * x + b * y == 1 -> Right x
      | otherwise ->
        Left $ "No modular inverse for " ++ show a ++ " and " ++ show b

chineseRemainder :: (Show b, Integral b) => [b] -> [b] -> Either [Char] b
chineseRemainder residues modulii =
  zipWithM modInv crtModulii modulii
    >>= (Right . (`mod` modPI) . sum . zipWith (*) crtModulii . zipWith (*) residues)
  where
    modPI = product modulii
    crtModulii = (modPI `div`) <$> modulii

partOne :: Integer -> [(Integer, Integer)] -> Integer
partOne = ((uncurry (*) . minimum) .) . map . (`ap` snd) . (. fst) . const . join . ((,) .) . mod . negate

partTwo :: (Show b, Integral b) => [(b, b)] -> b
partTwo = fromRight 0 . ap (chineseRemainder . map (uncurry subtract)) (map snd)

main =
  liftM2
    (>>)
    (print . uncurry partOne)
    (print . partTwo . snd)
    . parseInput
    . lines
    =<< readFile "input.txt"