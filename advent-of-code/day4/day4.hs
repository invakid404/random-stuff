import           Control.Monad       (liftM2)
import           Data.Char           (isDigit, isHexDigit)
import qualified Data.HashMap.Strict as M
import           Data.List.Split     (chunksOf, splitOn)
import           Data.Maybe          (isJust)
import           Text.Read           (readMaybe)

type Passport = M.HashMap String String

readLines :: FilePath -> IO [String]
readLines = (lines `fmap`) . readFile

joinLines :: [String] -> [String]
joinLines = map unwords . splitOn [""]

pairAdjacent :: Show b => [b] -> [(b, b)]
pairAdjacent = map (\[x, y] -> (x, y)) . chunksOf 2

readFirst :: String -> (Int, String)
readFirst = readFirst' ""
  where
    readFirst' acc "" = (read acc, "")
    readFirst' acc (c : cs)
      | isDigit c = readFirst' (acc ++ [c]) cs
      | otherwise = (read acc, c : cs)

parsePassport :: String -> Passport
parsePassport = M.fromList . pairAdjacent . concatMap (splitOn ":") . words

partOne :: [Passport] -> [Passport]
partOne = filter (all (True ==) . flip map requiredFields . flip M.member)
  where
    requiredFields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

partTwo :: [Passport] -> [Passport]
partTwo = filter (all (uncurry (((True ==) .) . validateField)) . M.toList) . partOne
  where
    validateField fieldName fieldValue
      | fieldName == "byr" = isInRange 1920 2002 (read fieldValue)
      | fieldName == "iyr" = isInRange 2010 2020 (read fieldValue)
      | fieldName == "eyr" = isInRange 2020 2030 (read fieldValue)
      | fieldName == "hgt" = validateHeight fieldValue
      | fieldName == "hcl" = head fieldValue == '#' && all isHexDigit (tail fieldValue)
      | fieldName == "ecl" = fieldValue `elem` eyeColors
      | fieldName == "pid" = length fieldValue == 9 && isJust (readMaybe fieldValue :: Maybe Int)
      | otherwise = True
      where
        eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
        validateHeight = uncurry validateHeight' . readFirst
          where
            validateHeight' hVal hType
              | hType == "cm" = isInRange 150 193 hVal
              | hType == "in" = isInRange 59 76 hVal
              | otherwise = False

    isInRange lo hi val = val >= lo && val <= hi

main :: IO ()
main = liftM2 (>>) (print . length . partOne) (print . length . partTwo) . map parsePassport . joinLines =<< readLines "input.txt"
