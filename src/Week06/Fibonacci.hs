{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Week06.Fibonacci
  ( fib,
    fibs1,
    fibs2,
    Stream (..),
    streamToList,
    streamRepeat,
    streamMap,
    streamFromSeed,
    nats,
    ruler,
    fibs3,
    fib4,
  )
where

---------------------------  Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n | n > 1 = fib (n -1) + fib (n -2)
fib n = - fib (- n)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

--------------------------- Exercise 2

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

--------------------------- Exercise 3

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream el xs) = el : streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

--------------------------- Exercise 4

streamRepeat :: a -> Stream a
streamRepeat n = Stream n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream el xs) = Stream (f el) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Stream seed (streamFromSeed f (f seed))

--------------------------- Exercise 5

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

ruler :: Stream Integer
ruler = rulerHelper 0

rulerHelper :: Integer -> Stream Integer
rulerHelper n = interleaveStreams (streamRepeat n) (rulerHelper $ n + 1)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Stream n s1) s2 = Stream n (interleaveStreams s2 s1)

--------------------------- Exercise 6

x :: Stream Integer
x = Stream 0 $ Stream 1 (streamRepeat 0)

instance Num (Stream Integer) where
  fromInteger n = Stream n $ streamRepeat 0
  negate (Stream n s) = Stream (- n) $ negate s
  (Stream n1 s1) + (Stream n2 s2) = Stream (n1 + n2) (s1 + s2)
  (Stream n1 s1) * b@(Stream n2 s2) =
    Stream (n1 * n2) (streamMap (* n1) s2 + s1 * b)

instance Fractional (Stream Integer) where
  a@(Stream n1 s1) / b@(Stream n2 s2) =
    Stream (n1 `div` n2) (streamMap (`div` n2) (s1 - s2 * a / b))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x ^ 2)

--------------------------- Exercise 7

data Matrix = Matrix
  { _x11 :: Integer,
    _x12 :: Integer,
    _x21 :: Integer,
    _x22 :: Integer
  }

instance Num Matrix where
  (Matrix x11_1 x12_1 x21_1 x22_1) * (Matrix x11_2 x12_2 x21_2 x22_2) =
    Matrix
      (x11_1 * x11_2 + x12_1 * x21_2)
      (x11_1 * x12_2 + x12_1 * x22_2)
      (x21_1 * x11_2 + x22_1 * x21_2)
      (x21_1 * x12_2 + x22_1 * x22_2)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n | n > 0 = _x12 $ Matrix 1 1 1 0 ^ n
fib4 n = - fib4 (- n)
