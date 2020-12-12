import Control.Monad

data Move
  = N Int
  | S Int
  | E Int
  | W Int
  | L Int
  | R Int
  | F Int
  deriving (Show, Read)

data Ship = Ship Int Int Int

data ShipTwo = ShipTwo Int Int Int Int

parseMove :: String -> Move
parseMove = read . unwords . (\(a, b) -> [a, b]) . splitAt 1

getMove :: Int -> (Int -> Move)
getMove 0 = E
getMove 90 = S
getMove 180 = W
getMove 270 = N

rotate :: (Eq a1, Num a1, Num a2) => a1 -> a2 -> a2 -> (a2, a2)
rotate 0 u v = (u, v)
rotate 90 u v = (v, - u)
rotate 180 u v = (- u, - v)
rotate 270 u v = (- v, u)

solve :: t1 -> (t2 -> t1 -> t1) -> (t1 -> p) -> [t2] -> p
solve ship m d (x : xs) = solve (m x ship) m d xs
solve ship _ d [] = d ship

partOne :: [Move] -> Int
partOne = solve (Ship 0 0 0) moveOne distOne
  where
    moveOne (N k) (Ship x y f) = Ship x (y + k) f
    moveOne (S k) (Ship x y f) = Ship x (y - k) f
    moveOne (E k) (Ship x y f) = Ship (x + k) y f
    moveOne (W k) (Ship x y f) = Ship (x - k) y f
    moveOne (F k) (Ship x y f) = moveOne (getMove f k) (Ship x y f)
    moveOne (R k) (Ship x y f) = Ship x y ((f + k) `mod` 360)
    moveOne (L k) (Ship x y f) = Ship x y ((f + 360 - k) `mod` 360)

    distOne (Ship x y _) = abs x + abs y

partTwo :: [Move] -> Int
partTwo = solve (ShipTwo 0 0 10 1) moveTwo distTwo
  where
    moveTwo (N k) (ShipTwo x y u v) = ShipTwo x y u (v + k)
    moveTwo (S k) (ShipTwo x y u v) = ShipTwo x y u (v - k)
    moveTwo (E k) (ShipTwo x y u v) = ShipTwo x y (u + k) v
    moveTwo (W k) (ShipTwo x y u v) = ShipTwo x y (u - k) v
    moveTwo (F k) (ShipTwo x y u v) = ShipTwo (x + u * k) (y + v * k) u v
    moveTwo (R k) (ShipTwo x y u v) = ShipTwo x y u' v' where (u', v') = rotate k u v
    moveTwo (L k) (ShipTwo x y u v) = ShipTwo x y u' v' where (u', v') = rotate (360 - k) u v

    distTwo (ShipTwo x y _ _) = abs x + abs y

main :: IO ()
main =
  liftM2
    (>>)
    (print . partOne)
    (print . partTwo)
    . map parseMove
    . lines
    =<< readFile "input.txt"