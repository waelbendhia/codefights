module RainbowOfClarity
  ( isDigit
  , lineEncoding
  ) where

import Data.List

isDigit :: Char -> Bool
isDigit symbol = any (== symbol) ['0' .. '9']

lineEncoding :: [Char] -> [Char]
lineEncoding s =
  group s >>= \l ->
    if length l > 1
      then show (length l) ++ [head l]
      else l
