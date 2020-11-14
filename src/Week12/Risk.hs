{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Week12.Risk where

import Control.Monad.Random
import Data.Functor ((<&>))
import Data.List
  ( sort,
  )

------------------------------------------------------------
-- Die values
newtype DieValue = DV
  { unDV :: Int
  }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk
type Army = Int

data Battlefield = Battlefield
  { attackers :: Army,
    defenders :: Army
  }
  deriving (Show, Eq)

------------------------------------------------------------
-- Exercise 1
-- This is just installing the monad random module, you don't
-- have to worry about this due to the magic of nix

------------------------------------------------------------
-- Exercise 2

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield att def) = do
  let f x = reverse . sort <$> replicateM x die
  attD <- f $ min 3 . max 0 $ att - 1
  defD <- f $ min 2 def
  let (attW, defW) = span id . sort $ zipWith (>) attD defD
  return $ Battlefield (att - length defW) (def - length attW)

------------------------------------------------------------
-- Exercise 3

invade :: Battlefield -> Rand StdGen Battlefield
invade b@(Battlefield att def)
  | att < 2 || def == 0 = return b
  | otherwise = battle b >>= invade

------------------------------------------------------------
-- Exercise 4

successProb :: Battlefield -> Rand StdGen Double
successProb b =
  replicateM 1000 (invade b)
    <&> ((/ 1000) . fromIntegral . length . filter ((== 0) . defenders))

------------------------------------------------------------
-- Exercise 5

-- Anyone know probability theory :p

exactSuccessProb :: Battlefield -> Double
exactSuccessProb (Battlefield att def)
  | att < 2 = 0
  | def == 0 = 1
  | otherwise = 0.5 -- to do later
