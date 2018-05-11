module Challenges.NthDerivative
  ( nthDerivative
  ) where

-- It's ugly but it clocks in at 159 chars
nthDerivative l = c . d l

z' = zipWith

b x = mod x $ 10 ^ 9 + 7

p x y = b (b x * b y)

c l x = s $ z' p l $ map (bp x) [0 ..]

s [] = 0
s (i:l) = b $ b i + s l

d [] _ = []
d l 0 = l
d l n = d (z' p (tail l) [1 ..]) $ n - 1

bp x 0 = 1
bp x n = b $ x * bp x (n - 1)
