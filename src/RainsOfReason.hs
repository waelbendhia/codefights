module RainsOfReason
  ( arrayReplace
  , evenDigitsOnly
  , variableName
  , alphabeticShift
  , chessBoardCellColor
  ) where

import Data.Char

arrayReplace :: (Num b, Eq b) => [b] -> b -> b -> [b]
arrayReplace inputArray elemToReplace substitutionElem =
  map
    (\e ->
       if e == elemToReplace
         then substitutionElem
         else e)
    inputArray

evenDigitsOnly :: Integral t => t -> Bool
evenDigitsOnly n
  | n < 10 = even n
  | even $ n `rem` 10 = evenDigitsOnly $ n `div` 10
  | otherwise = False

variableName :: [Char] -> Bool
variableName [] = False
variableName name = (not $ inBound '0' '9' $ head name) && all checker name
  where
    inBound cLo cHi c = ord c >= ord cLo && ord c <= ord cHi
    checker c =
      any
        (\f -> f c)
        [inBound 'a' 'z', inBound 'A' 'Z', inBound '0' '9', (== '_')]

alphabeticShift :: [Char] -> [Char]
alphabeticShift = map incChar

incChar :: Char -> Char
incChar 'z' = 'a'
incChar c = chr $ ord c + 1

chessBoardCellColor :: [Char] -> [Char] -> Bool
chessBoardCellColor cell1 cell2 = even $ dist (getVal cell1) (getVal cell2)
  where
    getVal [r, c] = (ord r - ord 'A', digitToInt c)
    dist (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
