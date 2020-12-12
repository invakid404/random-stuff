import Control.Arrow (Arrow (second))
import Control.Monad (liftM2, (<=<))
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)

type Position = (Int, Int)

data Seat = None | Empty | Occupied deriving (Eq, Show)

type Room = M.Map Position Seat

index2D :: [[b]] -> [((Int, Int), b)]
index2D = uncurry (flip zipWith [0 ..] . ((,) .) . flip (,)) <=< zip [0 ..]

until2 :: (a -> a -> Bool) -> (a -> a) -> a -> a
until2 p f a
  | p a a' = a
  | otherwise = until2 p f a'
  where
    a' = f a

parseRoom :: [[Char]] -> Room
parseRoom = M.fromList . map (second parseSeat) . index2D
  where
    parseSeat '.' = None
    parseSeat 'L' = Empty
    parseSeat '#' = Occupied

directions :: [Position]
directions = [(-1, -1), (0, -1), (1, -1), (-1, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]

addPositions :: Position -> Position -> Position
addPositions (x, y) (x', y') = (x + x', y + y')

scan :: Position -> Room -> Position -> Maybe Seat
scan p r d =
  case M.lookup p' r of
    Just None -> scan p' r d
    seat -> seat
  where
    p' = addPositions p d

simulate :: Int -> (Position -> Room -> Int) -> Room -> Room
simulate n f r = M.fromList [(p, s') | (p, s) <- M.assocs r, let s' = simulateOne n s (f p r)]
  where
    simulateOne _ Empty 0 = Occupied
    simulateOne n Occupied c | c >= n = Empty
    simulateOne _ s _ = s

countOccupied :: [Seat] -> Int
countOccupied = length . filter (== Occupied)

solve :: Int -> (Position -> Room -> Int) -> Room -> Int
solve n f = countOccupied . M.elems . until2 (==) (simulate n f)

partOne :: Room -> Int
partOne = solve 4 (flip flip directions . (((countOccupied .) . mapMaybe) .) . flip ((.) . flip M.lookup) . addPositions)

partTwo :: Room -> Int
partTwo = solve 5 (flip flip directions . (((countOccupied .) . mapMaybe) .) . scan)

main :: IO ()
main =
  liftM2
    (>>)
    (print . partOne)
    (print . partTwo)
    . parseRoom
    . lines
    =<< readFile "input.txt"