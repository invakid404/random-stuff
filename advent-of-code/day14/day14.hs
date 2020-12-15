{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (liftM2)
import Data.Attoparsec.Text.Lazy
  ( Parser,
    choice,
    decimal,
    parseOnly,
    string,
    takeText,
  )
import Data.Bits (Bits (clearBit, setBit))
import Data.Either (rights)
import qualified Data.Map.Strict as M
import qualified Data.Text as T

data Instruction = SetMask T.Text | SetMem Int Int deriving (Show)

parseInstruction :: Parser Instruction
parseInstruction = choice [parseSetMem, parseSetMask]

parseSetMask :: Parser Instruction
parseSetMask = do
  string "mask = "
  SetMask <$> takeText

parseSetMem :: Parser Instruction
parseSetMem = do
  string "mem["
  idx <- decimal
  string "] = "
  SetMem idx <$> decimal

partOne :: [Instruction] -> Int
partOne = go id M.empty
  where
    go f map (SetMem i n : rest) = go f (M.insert i (f n) map) rest
    go _ map code = case code of
      (SetMask s : rest) -> go (maskOne (T.unpack s)) map rest
      _ -> M.foldr (+) 0 map

maskOne :: String -> Int -> Int
maskOne = go [35, 34 ..]
  where
    go (i : is) ('0' : bs) = flip clearBit i . go is bs
    go (i : is) ('1' : bs) = flip setBit i . go is bs
    go (_ : is) ('X' : bs) = go is bs
    go _ [] = id

partTwo :: [Instruction] -> Int
partTwo = go (: []) M.empty
  where
    go f map (SetMem i n : rest) = go f (foldr (`M.insert` n) map (f i)) rest
    go _ map code = case code of
      (SetMask s : rest) -> go (maskTwo (T.unpack s)) map rest
      _ -> M.foldr (+) 0 map

maskTwo :: String -> Int -> [Int]
maskTwo = (. return) . go [35, 34 ..]
  where
    go (_ : is) ('0' : bs) = go is bs
    go (i : is) ('1' : bs) = map (`setBit` i) . go is bs
    go (i : is) ('X' : bs) = \ns -> [f n i | f <- [setBit, clearBit], n <- go is bs ns]
    go _ [] = id

main :: IO ()
main =
  liftM2
    (>>)
    (print . partOne)
    (print . partTwo)
    . rights
    . map (parseOnly parseInstruction . T.pack)
    . lines
    =<< readFile "input.txt"