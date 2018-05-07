module Intro
  ( add
  , centuryFromYear
  , checkPalindrome
  ) where

import Data.List

add :: Integer -> Integer -> Integer
add = (+)

centuryFromYear :: Integral a => a -> a
centuryFromYear year = (year `div` 100) + (min 1 $ year `rem` 100)

checkPalindrome :: String -> Bool
checkPalindrome x = start == end
  where
    len = length x `div` 2
    start = take len x
    end = take len $ reverse $ x
