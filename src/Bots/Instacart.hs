module Bots.Instacart
  (
  ) where

import Data.Char
import Data.List

delivery order shoppers = map conv $ map (map fromIntegral) shoppers
  where
    [dist, readyIn, waitTime] = map fromIntegral order
    conv [distS, speed, timeInShop] =
      let delivered = ((dist + distS) / speed) + timeInShop
      in delivered >= readyIn && delivered - readyIn <= waitTime

isAdmissibleOverpayment prices notes x =
  x >= (sum $ zipWith ($) (map noteToFunc notes) prices)

noteToFunc note =
  if length perc == length note
    then (const 0)
    else (\p -> p - (p * 100 / (100 + sign (read perc))))
  where
    perc = takeWhile (/= '%') note
    sign =
      if isSubsequenceOf "higher" note
        then id
        else (* (-1))

busyHolidays shoppers orders leadTime =
  busyHolidays'
    (map intervalToTime shoppers)
    (map intervalToTime orders)
    leadTime

busyHolidays' _ [] _ = True
busyHolidays' [] _ _ = False
busyHolidays' shoppers (order:orders) (leadTime:leadTimes)
  | not $ any (\s -> canPickUp s order leadTime) shoppers = False
  | otherwise =
    or
      (map
         (\s -> busyHolidays' (delete s shoppers) orders leadTimes)
         validShoppers)
  where
    validShoppers = filter (\s -> canPickUp s order leadTime) shoppers

canPickUp (sStart, sEnd) (oStart, oEnd) t = iEnd - iStart >= t
  where
    (iStart, iEnd) = (max sStart oStart, min sEnd oEnd)

intervalToTime :: [[Char]] -> (Int, Int)
intervalToTime [s, e] = (stringToTime s, stringToTime e)

stringToTime :: [Char] -> Int
stringToTime [h1, h2, _, m1, m2] =
  digitToInt h1 * 600 + digitToInt h2 * 60 + digitToInt m1 * 10 + digitToInt m2
