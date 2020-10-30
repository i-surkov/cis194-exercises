module Week01.CreditCardValidator
  ( toDigits,
    toDigitsRev,
    doubleEveryOther,
    sumDigits,
    validate,
  )
where

-- | Converts Integer into array of digits
toDigits :: Integer -> [Integer]
toDigits x = reverse (toDigitsRev x)

-- | Converts Integer into reversed array of digits
toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x > 0 = x `mod` 10 : toDigitsRev (x `div` 10)
  | otherwise = []

-- | Doubles every other element starting from the end
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst = reverse (doubleHelper (reverse lst))
  where
    doubleHelper (x1 : x2 : rest) = [x1, x2 * 2] ++ doubleHelper rest
    doubleHelper l = l

-- | Sums all digits in array
sumDigits :: [Integer] -> Integer
sumDigits lst = sum [if x < 10 then x else x `mod` 10 + x `div` 10 | x <- lst]

-- | Checks if Integer is a valid credit card number
validate :: Integer -> Bool
validate x = (sumDigits (doubleEveryOther (toDigits x))) `mod` 10 == 0
