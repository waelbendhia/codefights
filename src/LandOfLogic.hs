module LandOfLogic
  ( longestWord
  , validTime
  , sumUpNumbers
  , differentSquares
  , digitsProduct
  , messageFromBinaryCode
  , spiralNumbers
  , sudoku
  ) where

import Data.Char
import Data.List

longestWord :: String -> [Char]
longestWord = maximumBy (\a b -> compare (length a) (length b)) . words'

words' :: String -> [[Char]]
words' = f ""
  where
    f acc [] = [reverse acc]
    f acc (c:cs) =
      if isAlpha c
        then f (c : acc) cs
        else (reverse acc) : f [] cs

validTime :: [Char] -> Bool
validTime time = h < 24 && m < 60
  where
    (h, m) = toTime time

toTime :: [Char] -> (Int, Int)
toTime [h1, h2, _, m1, m2] =
  (digitToInt h1 * 10 + digitToInt h2, digitToInt m1 * 10 + digitToInt m2)

sumUpNumbers :: [Char] -> Int
sumUpNumbers = sum . numbers

numbers :: (Read a, Integral a) => [Char] -> [a]
numbers = fmap read . f ""
  where
    f acc [] =
      if acc /= ""
        then [reverse acc]
        else []
    f acc (c:cs) =
      if isDigit c
        then f (c : acc) cs
        else if acc /= ""
               then (reverse acc) : f [] cs
               else f [] cs

differentSquares :: (Num d, Ord d) => [[d]] -> Int
differentSquares matrix =
  length $
  group $
  sort $ do
    (l1, l2) <- groupPairs matrix
    ((x1, x2), (x3, x4)) <- groupPairs $ zip l1 l2
    return (x1, x2, x3, x4)

groupPairs :: [a] -> [(a, a)]
groupPairs (x1:x2:xs) = (x1, x2) : groupPairs (x2 : xs)
groupPairs _ = []

digitsProduct :: (Read p, Num p) => Int -> p
digitsProduct 0 = 10
digitsProduct 1 = 1
digitsProduct n
  | length divs == 0 || rem > 1 = -1
  | otherwise = read $ intToDigit <$> divs
  where
    (divs, rem) =
      foldl
        (\(divs, n') d ->
           let (nextD, nextN) = divIter' d n'
           in (nextD ++ divs, nextN))
        ([], n)
        [9,8 .. 2]

divIter' :: Integral p => p -> p -> ([p], p)
divIter' d n = (ds, n `div` product ds)
  where
    ds = divIter d n

divIter :: Integral a => a -> a -> [a]
divIter d n =
  unfoldr
    (\n' ->
       if n' `rem` d == 0
         then Just (d, n' `div` d)
         else Nothing)
    n

messageFromBinaryCode :: [Char] -> [Char]
messageFromBinaryCode [] = ""
messageFromBinaryCode code = firstChar : messageFromBinaryCode rest
  where
    (c, rest) = splitAt 8 code
    firstChar =
      chr $ sum $ zipWith (*) (map (2 ^) [0 ..]) $ reverse $ map digitToInt $ c

spiralNumbers :: (Enum a, Num a, Eq a) => a -> [[a]]
spiralNumbers 1 = [[1]]
spiralNumbers n = [1 .. n] : next
  where
    f f' x = map (map f') x
    next =
      reverse <$>
      (reverse $
       zipWith (:) [n * 2 - 1,n * 2 - 2 ..] <$> f (+ (2 * n - 1)) $
       spiralNumbers $ n - 1)

sudoku :: (Num a, Ord a) => [[a]] -> Bool
sudoku grid = and $ validate <$> blocks ++ grid ++ lines grid
  where
    validate l = (== 9) $ length $ group $ sort l
    blocks = concat $ zipTriples $ toTriplets <$> grid
    lines [l] = map (: []) l
    lines (l:ls) = zipWith (:) l $ lines ls

toTriplets (x1:x2:x3:xs) = [x1, x2, x3] : toTriplets xs
toTriplets _ = []

zipTriples (l1:l2:l3:ls) =
  zipWith3 (\x y z -> x ++ y ++ z) l1 l2 l3 : zipTriples ls
zipTriples _ = []
