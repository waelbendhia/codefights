module RainbowOfClarity
  ( isDigit
  , lineEncoding
  , chessKnight
  , deleteDigit
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

chessKnight :: [Char] -> Int
chessKnight cell =
  length $
  filter (\(x, y) -> x >= 0 && x < 8 && y >= 0 && y < 8) $ nextPos $ getVal cell
  where
    nextPos (x, y) =
      [ (x + 2, y + 1)
      , (x + 1, y + 2)
      , (x - 2, y + 1)
      , (x - 1, y + 2)
      , (x + 2, y - 1)
      , (x + 1, y - 2)
      , (x - 2, y - 1)
      , (x - 1, y - 2)
      ]

getVal :: Enum a => [a] -> (Int, Int)
getVal [r, c] = (fromEnum r - fromEnum 'a', fromEnum c - fromEnum '1')

deleteDigit :: (Show a, Read a, Integral a) => a -> a
deleteDigit n = maximum $ read <$> variations [] (show n)
  where
    variations _ [] = []
    variations acc (c:cs) = (acc ++ cs) : variations (acc ++ [c]) cs
