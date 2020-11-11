{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}

module Week07.JoinList
  ( JoinList (..),
    joinListToList,
    (!!?),
    (+++),
    indexJ,
    dropJ,
    takeJ,
    scoreLine,
    tag,
    main,
  )
where

import Week07.Buffer (Buffer (..))
import Week07.Editor
  ( editor,
    runEditor,
  )
import Week07.Scrabble
  ( Score (..),
    scoreString,
  )
import Week07.Sized
  ( Size (..),
    Sized (..),
    getSize,
  )

data JoinList m a
  = Empty
  | Single
      m
      a
  | Append
      m
      (JoinList m a)
      (JoinList m a)
  deriving (Eq, Show)

joinListToList :: JoinList m a -> [a]
joinListToList Empty = []
joinListToList (Single _ a) = [a]
joinListToList (Append _ l r) = joinListToList l ++ joinListToList r

(!!?) :: Int -> [a] -> Maybe a
(!!?) n = lookup n . zip [0 ..]

--------------------------- Exercise 1

-- Suggestion (no tests):
-- Pulls the monoidal value out of the root of the JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2 = Append (tag jl1 <> tag jl2) jl1 jl2

--------------------------- Exercise 2

getJlSize :: (Sized m, Monoid m) => JoinList m a -> Int
getJlSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ _ (Single _ x) = Just x
indexJ n (Append m left right)
  | n < 0 = Nothing
  | n >= (getSize . size) m = Nothing
  | n >= leftSize = indexJ (n - leftSize) right
  | otherwise = indexJ n left
  where
    leftSize = getJlSize left

-- | Common logic for dropJ and takeJ, the only difference is how they
-- handle edge cases, for which they provide provide the first argument -
-- function returning a tuple of values for what to do in case:
-- 1. if index is larger than size of the list
-- 2. if index is `>= 0`
sliceJ ::
  (Sized b, Monoid b) =>
  (JoinList b a -> (JoinList b a, JoinList b a)) ->
  Int ->
  JoinList b a ->
  JoinList b a
sliceJ f n jl
  | n >= getJlSize jl = fst . f $ jl
  | n <= 0 = snd . f $ jl
sliceJ f n (Append _ left right) = newLeft +++ newRight
  where
    newLeft = sliceJ f n left
    newRight = sliceJ f (n - getJlSize left) right
sliceJ _ _ jl = jl -- exhausting patter match, even though wouldn't be reached

-- | Applying common logic, we return empty list of we need to drop more
-- or equal to the size of the list, and return the whole list if we need
-- to drop `<=0` elements
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ = sliceJ (Empty,)

-- | Applying common logic, we return whole list of we need to take more
-- or equal to the size of the list, and return empty list if we need
-- to take `<=0` elements
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ = sliceJ (,Empty)

--------------------------- Exercise 3

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreString str) str

--------------------------- Exercise 4

instance Buffer (JoinList (Score, Size) String) where
  toString :: JoinList (Score, Size) String -> String
  toString Empty = ""
  toString (Single _ s) = s
  toString (Append _ left right) = toString left ++ "\n" ++ toString right

  fromString :: String -> JoinList (Score, Size) String
  fromString = fromLines . lines
    where
      fromLines [] = Empty
      fromLines [str] = Single (scoreString str, Size 1) str
      fromLines xs =
        let n = (`div` 2) . length $ xs
         in fromLines (take n xs) +++ fromLines (drop n xs)

  line :: Int -> JoinList (Score, Size) String -> Maybe String
  line = indexJ

  replaceLine ::
    Int ->
    String ->
    JoinList (Score, Size) String ->
    JoinList (Score, Size) String
  replaceLine _ _ Empty = Empty
  replaceLine _ str (Single _ _) = fromString str
  replaceLine n str jl@(Append m left right)
    | n < 0 = jl
    | n >= (getSize . size) m = jl
    | n >= leftSize = left +++ replaceLine (n - leftSize) str right
    | otherwise = replaceLine n str left +++ right
    where
      leftSize = getJlSize left

  numLines :: JoinList (Score, Size) String -> Int
  numLines = getJlSize

  value :: JoinList (Score, Size) String -> Int
  value = getScore . fst . tag

initialValue :: JoinList (Score, Size) String
initialValue =
  fromString $
    unlines
      [ "This buffer is for notes you don't want to save, and for",
        "evaluation of steam valve coefficients.",
        "To load a different file, type the character L followed",
        "by the name of the file."
      ]

main :: IO ()
main = runEditor editor initialValue
