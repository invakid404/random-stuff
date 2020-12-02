{-# LANGUAGE OverloadedStrings #-}

import Data.Attoparsec.Text
  ( Parser,
    char,
    decimal,
    letter,
    parseOnly,
    space,
    takeText,
  )
import Data.Either (rights)
import qualified Data.Text as T

data Password = Password {min :: Int, max :: Int, key :: Char, pass :: T.Text}

readLines :: FilePath -> IO [T.Text]
readLines = ((T.lines . T.pack) `fmap`) . readFile

solve :: (a -> Bool) -> [a] -> Int
solve = (length .) . filter

part1 :: [Password] -> Int
part1 = solve isValid
  where
    isValid (Password min max key pass) = isInRange min max $ T.length $ T.filter (key ==) pass
    isInRange x y n = n >= x && n <= y

part2 :: [Password] -> Int
part2 = solve isValid
  where
    isValid (Password min max key pass) = (T.index pass (min - 1) == key) /= (T.index pass (max - 1) == key)

parsePassword :: Parser Password
parsePassword = do
  min <- decimal
  char '-'
  max <- decimal
  space
  key <- letter
  char ':'
  space
  password <- takeText
  return $ Password min max key password

main = do
  lines <- readLines "input.txt"
  let passwords = rights (map (parseOnly parsePassword) lines)
  print (part1 passwords)
  print (part2 passwords)