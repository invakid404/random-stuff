{-# LANGUAGE FlexibleContexts #-}

import           Control.Lens              (Ixed (ix), over)
import           Control.Monad             (liftM2, when)
import           Control.Monad.Except      (MonadError (throwError))
import           Data.Attoparsec.Text.Lazy (Parser, decimal, letter, many',
                                            parseOnly, signed, skipSpace)
import           Data.Either               (fromLeft, rights)
import qualified Data.HashSet              as S
import qualified Data.Text                 as T
import           Data.Vector               ((!?))
import qualified Data.Vector               as V

type Console = V.Vector Instruction

data Instruction
  = Acc Int
  | Jmp Int
  | Nop Int
  deriving (Eq, Show)

parseConsole :: [String] -> Console
parseConsole = V.fromList . rights . map (parseOnly instruction . T.pack)

instruction :: Parser Instruction
instruction = do
  opName <- many' letter
  skipSpace
  opArg <- signed decimal
  return $
    case opName of
      "acc" -> Acc opArg
      "jmp" -> Jmp opArg
      "nop" -> Nop opArg
      _     -> error $ "Unknown opname: " ++ opName

modify :: Instruction -> Instruction
modify (Jmp n) = Nop n
modify x       = x

run :: Console -> Either Int Int
run = go S.empty 0 0
 where
  go vis rip acc ops = do
    when (S.member rip vis) $ throwError acc
    let newVis = S.insert rip vis
    case ops !? rip of
      Just (Acc n) -> go newVis (rip + 1) (acc + n) ops
      Just (Jmp n) -> go newVis (rip + n) acc ops
      Just (Nop _) -> go newVis (rip + 1) acc ops
      Nothing      -> pure acc

partOne :: Console -> Int
partOne = fromLeft 0 . run

partTwo :: Console -> Int
partTwo console =
  head
    [ n | i <- [0 .. V.length console - 1]
    , Right n <- [run $ over (ix i) modify console]
    ]

main :: IO ()
main =
  liftM2 (>>)
    (print . partOne)
    (print . partTwo)
    . parseConsole
    . lines
    =<< readFile "input.txt"
