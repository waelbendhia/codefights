module EruptionOfLight
  ( isBeautifulString
  , findEmailDomain
  , buildPalindrome
  , electionsWinners
  , isMAC48Address
  ) where

import Data.Char

isBeautifulString :: [Char] -> Bool
isBeautifulString inputString = f occurs
  where
    occurs =
      map
        (\x -> length $ filter ((x ==) . subtract (ord 'a') . ord) inputString)
        [0 .. 25]
    f [] = True
    f [_] = True
    f (x1:x2:xs)
      | x1 >= x2 = f (x2 : xs)
      | otherwise = False

findEmailDomain :: [Char] -> [Char]
findEmailDomain = reverse . takeWhile (/= '@') . reverse

buildPalindrome :: String -> String
buildPalindrome st = f st []
  where
    isPalindrome s = s == reverse s
    f [] rev = st ++ rev
    f (c:cs) rev
      | isPalindrome (st ++ rev) = st ++ rev
      | otherwise = f cs (c : rev)

electionsWinners :: (Ord a, Num a) => [a] -> a -> Int
electionsWinners votes 0 =
  if (length $ filter (== maximum votes) votes) > 1
    then 0
    else 1
electionsWinners votes k = length $ filter ((> maximum votes) . (+ k)) votes

isMAC48Address :: [Char] -> Bool
isMAC48Address inputString = length splitMac == 6 && all checker splitMac
  where
    splitMac = splitOn '-' inputString
    checker part = length part == 2 && all isHex part

isHex :: Char -> Bool
isHex c = ord c - ord '0' <= 9 || ord c - ord 'A' <= 5

splitOn :: Eq p => p -> [p] -> [[p]]
splitOn y s = f [] [] s
  where
    f t acc (c:cs)
      | c == y = f (t ++ [acc]) [] cs
      | otherwise = f t (acc ++ [c]) cs
    f t acc [] = (t ++ [acc])
