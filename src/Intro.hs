module Intro
  ( add
  , centuryFromYear
  , checkPalindrome
  ) where

add :: Integer -> Integer -> Integer
add = (+)

centuryFromYear :: Integral a => a -> a
centuryFromYear year = (year `div` 100) + (min 1 $ year `rem` 100)

checkPalindrome :: Eq a => [a] -> Bool
checkPalindrome x = start == end
  where
    len = length x `div` 2
    start = take len x
    end = take len $ reverse $ x
