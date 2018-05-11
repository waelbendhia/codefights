module Challenges.MaxCardSequence
  ( maxCardSequence
  ) where

import Data.List

maxCardSequence = mcs . sort

mcs [] = 0
mcs [_] = 1
mcs (x1:x2:xs) =
  if odd x1 == odd x2
    then 0 + mcs (x1 : xs)
    else 1 + mcs (x2 : xs)
