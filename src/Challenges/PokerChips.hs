module PokerChips
  (
  ) where

pokerChips chips = rightT + leftT
  where
    avg = sum chips `div` length chips
    deficits = map (subtract avg) chips
    (rightT, rightL) = shiftC (head deficits) deficits
    (leftT, _) = shiftC (head $ reverse rightL) (reverse rightL)

shiftC acc [] = (0,[])
shiftC acc (x:xs) = ()

shiftC _ [] = (0, [])
shiftC first [x] =
  if x > 0 && first < 0
    then (x, [])
    else (0, [])
shiftC first (x1:x2:xs) =
  if x1 > 0 && x2 < 0
    then let (nextT, nextL) = shiftC first (x2 + x1 : xs)
         in (nextT + x1, 0 : nextL)
    else let (nextT, nextL) = shiftC first (x2 : xs)
         in (nextT, x1 : nextL)
