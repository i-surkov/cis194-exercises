module Week04.Soln
  ( fun1,
    fun2,
    fun1',
    fun2',
    Tree (..),
    foldTree,
    showTree,
    printTree,
    xor,
    map',
    myFoldl,
    cartProd,
    sieveSundaram,
  )
where

import qualified Data.Set as Set

---------------------------  Exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x : xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' =
  sum . filter even . takeWhile (/= 1)
    . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

---------------------------  Exercise 2

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

-- | Produce balanced binary tree from a list
foldTree :: [a] -> Tree a
-- Version with explicit lambda:
-- foldTree = foldr (\x tree -> fst $ insertTree x tree) Leaf
-- Version with chaining insertTree using curry-uncurry:
foldTree = foldr (curry $ fst . uncurry insertTree) Leaf

-- | Insert a value in a balanced tree, keeping it balanced, returning
-- updated tree and a `Bool` value indicating whether height of the tree
-- increased
insertTree :: a -> Tree a -> (Tree a, Bool)
insertTree x Leaf = (Node 0 Leaf x Leaf, True)
insertTree x (Node h left v right)
  | left `taller` right = (Node h left v (fst $ insertTree x right), False)
  | right `taller` left = (Node h resTree v right, False)
  | otherwise = (Node (if incr then h + 1 else h) resTree v right, incr)
  where
    taller (Node h1 _ _ _) (Node h2 _ _ _) = h1 > h2
    taller Node {} Leaf = True
    taller _ _ = False
    (resTree, incr) = insertTree x left

showTree :: Show a => Tree a -> String
showTree Leaf = ""
showTree n@(Node s _ _ _) = go s n
  where
    go _ Leaf = ""
    go i (Node h l c r) =
      go (i - 1) l
        ++ replicate (4 * fromIntegral i) ' '
        ++ show c
        ++ "-"
        ++ show h
        ++ "\n"
        ++ go (i - 1) r

-- will print a tree in ghci, root node will be the rightmost in the printed structure
-- nodes will be printed as [value]-[height]
printTree :: Show a => Tree a -> IO ()
printTree t = putStrLn $ showTree t

---------------------------  Exercise 3

-- | Return `True` if input array contains odd number of `True` values
xor :: [Bool] -> Bool
xor = foldr (\x acc -> if x then not acc else acc) False

-- | Map implemented using foldr
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- | Foldl implemented using foldr - chaining partially applied funcions
-- to all elements, and then applying `base` value to resulted function
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x ff -> ff . flip f x) id xs base

---------------------------  Exercise 4
-- See: https://en.wikipedia.org/wiki/Sieve_of_Sundaram
-- cartProd provided for you to use in your solution

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

-- | Return list of all odd primes up to `2n + 1`
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [2 * x + 1 | x <- [1 .. n], Set.notMember x set]
  where
    set =
      Set.fromList
        [a | i <- [1 .. n], j <- [1 .. i], let a = i + j + 2 * i * j, a <= n]
