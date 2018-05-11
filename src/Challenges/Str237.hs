module Challenges.Str237
  ( str237
  ) where

import Data.Char
import Data.List

-- In the example "Hello world!" sorted it becomes " !!Hdellloorw", for this 
-- string solving from 237 requires solving "!!Hdellloorw" for 205 and 237
str237 = f 237 . sort . map ord

-- Solve for 0 has solution ""
f 0 _ = 1
-- Solve for t in a SORTED string is solving tail string for t and tail string 
-- for head t.
f t (c:s)
  | c > t = 0 -- If the head of the list is higher than our desired t then we can just stop as our string is sorted
  | otherwise = f (t - c) s + f t s
f _ _ = 0
