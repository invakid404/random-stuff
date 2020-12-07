{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (ap, liftM2)
import Data.Attoparsec.Text.Lazy
  ( Parser,
    anyChar,
    choice,
    decimal,
    inClass,
    many',
    manyTill,
    parseOnly,
    skipSpace,
    string,
    takeWhile1,
  )
import Data.Either (rights)
import Data.Graph.Inductive.Graph
  ( Graph (mkGraph),
    LNode,
    Node,
    out,
  )
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.DFS (rdfs)
import qualified Data.HashMap.Lazy as M
import Data.Maybe (catMaybes, fromJust)
import qualified Data.Text as T

data Bags = Bags {gr :: Gr String Int, target :: Node}

parseSentence :: Parser ([Char], [([Char], Int)])
parseSentence = do
  key <- manyTill anyChar (string " bags contain ")
  values <- many' parseValue
  return (key, catMaybes values)

parseValue :: Parser (Maybe ([Char], Int))
parseValue = choice [parseAmount, parseNone]

parseNone :: Parser (Maybe a)
parseNone = do
  string "no other bags"
  return Nothing

parseAmount :: Parser (Maybe ([Char], Int))
parseAmount = do
  amount <- decimal
  skipSpace
  value <- manyTill anyChar (string " bag")
  takeWhile1 (inClass " s.,\n")
  return $ Just (value, amount)

buildHashMap :: [(String, [(String, Int)])] -> M.HashMap String (LNode String)
buildHashMap = M.fromList . flip (zipWith (liftM2 (.) (,) (flip (,))) . map fst) [0 ..]

buildEdges :: [(a1, [(a2, a3)])] -> [(a1, a2, a3)]
buildEdges = map (uncurry ((`ap` snd) . (. fst) . (,,))) . (uncurry (zip . repeat) =<<)

buildGraph :: [(String, [(String, Int)])] -> Bags
buildGraph d = Bags (mkGraph n g) (get "shiny gold" m)
  where
    get = ((fst . fromJust) .) . M.lookup
    m = buildHashMap d
    e = buildEdges d
    n = M.elems m
    g = map (\(x, y, z) -> (get x m, get y m, z)) e

partOne :: Bags -> Int
partOne (Bags gr target) = length (rdfs [target] gr) - 1

partTwo :: Bags -> Int
partTwo (Bags gr target) = partTwo' gr target 1
  where
    partTwo' = ((sum .) .) . ap ((.) . flip . (map .) . recurse) out
    recurse g m (_, to, w) = n + partTwo' g to n
      where
        n = w * m

main :: IO ()
main =
  liftM2
    (>>)
    (print . partOne)
    (print . partTwo)
    . buildGraph
    . rights
    . map (parseOnly parseSentence . T.pack)
    . lines
    =<< readFile "input.txt"