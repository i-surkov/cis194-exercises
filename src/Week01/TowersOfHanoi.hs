module Week01.TowersOfHanoi
  ( hanoi,
    hanoi4,
  )
where

type Peg = String

type Move = (Peg, Peg)

-- | Solves the tower of hanoi problem for 3 pegs, given `x` number of disks
-- and names of the three pegs
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi x a b c
  | x == 1 = [(a, b)]
  | x > 1 = hanoi (x - 1) a c b ++ [(a, b)] ++ hanoi (x - 1) c b a
  | otherwise = []

-- | Solves the tower of hanoi problem for 4 pegs, given `x` number of disks
-- and name of the four pegs
hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 x a b c d
  | x > 1 = hanoi4 (x - k) a d c b ++ hanoi k a b c ++ hanoi4 (x - k) d b a c
  | otherwise = hanoi x a b c
  where
    -- `k` is an optimal number of disks to move using 3 pegs,
    -- after moving @x - k@ disks from the top of the pyramid
    k = round (sqrt (2 * fromIntegral x + 1) :: Double) - 1