{-# LANGUAGE InstanceSigs #-}

module Week10.AParser
  ( Parser (..),
    satisfy,
    char,
    posInt,
    abParser,
    abParser_,
    intPair,
    intOrUppercase,
  )
where

import Control.Applicative (Alternative (..))
import Data.Bifunctor (Bifunctor (first))
import Data.Char ( isDigit, isUpper )
import Control.Monad.Cont ((>=>))
import Data.Functor (void)
import GHC.Base (liftA3)
import GHC.Unicode (isSpace)

newtype Parser a = Parser
  { runParser :: String -> Maybe (a, String)
  }

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x : xs)
      | p x = Just (x, xs)
      | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns = Nothing
      | otherwise = Just (read ns, rest)
      where
        (ns, rest) = span isDigit xs

---------------------------  Exercise 1

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ fmap (first f) . p

---------------------------  Exercise 2

instance Applicative Parser where
  -- | Parser that always return provided value and empty string
  pure :: a -> Parser a
  pure x = Parser $ \s -> Just (x, s)

  -- | Parser, that runs the first parser, which returns a function, and then
  -- runs the second parser on the input, that was left by the first one,
  -- and applies first parser function to the second parser result value
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (Parser p1) <*> p2 = Parser $ p1 >=> \(f, s) -> runParser (f <$> p2) s

---------------------------  Exercise 3

abParser :: Parser (Char, Char)
abParser = (,) <$> satisfy (== 'a') <*> satisfy (== 'b')

abParser_ :: Parser ()
abParser_ = void abParser

intPair :: Parser [Int]
intPair = liftA3 (\a _ b -> map fromInteger [a,b])
            posInt (satisfy isSpace) posInt

---------------------------  Exercise 4

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (Parser p1) <|> (Parser p2) = Parser $ \s -> p1 s <|> p2 s

---------------------------  Exercise 5

intOrUppercase :: Parser ()
intOrUppercase = void posInt <|> void (satisfy isUpper)
