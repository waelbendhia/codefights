module ThroughTheFog
  ( circleOfNumbers
  , depositProfit
  , absoluteValuesSumMinimization
  , stringsRearrangement
  ) where

import Data.Char
import Data.List

circleOfNumbers :: Integral a => a -> a -> a
circleOfNumbers n = (`mod` n) . (+ (n `div` 2))

depositProfit ::
     (Num p, Integral a3, Integral a2, Integral a1) => a1 -> a2 -> a3 -> p
depositProfit deposit rate threshold =
  f (fromIntegral deposit) (fromIntegral rate) (fromIntegral threshold)
  where
    f d r t
      | d >= t = 0
      | otherwise = 1 + f (d * (100 + r) / 100) r t

absoluteValuesSumMinimization :: (Num b, Ord b) => [b] -> b
absoluteValuesSumMinimization a =
  fst $
  minimumBy
    (\(a1, a2) (b1, b2) ->
       if a2 == b2
         then compare a1 b1
         else compare a2 b2) $
  map (\x -> (x, sum $ map (abs . (-) x) a)) a

-- This looks like it runs in factorial time in the worst case because it does!
-- But the problem says the list size is below or equal to 10 so it's all good.
-- You could vastly speed this up by memoizing already calculated subsets.
stringsRearrangement :: [[Char]] -> Bool
stringsRearrangement inputArray =
  any (\c -> f c $ delete c inputArray) inputArray
  where
    stringDif s1 s2 =
      sum $
      zipWith
        (\c1 c2 ->
           if c1 == c2
             then 0
             else 1)
        s1
        s2
    f s1 [s2] = stringDif s1 s2 == 1
    f s1 ss = any (\c -> f c $ delete c ss) $ filter ((== 1) . stringDif s1) ss
