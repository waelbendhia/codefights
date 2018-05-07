module DarkWilderness
  ( growingPlant
  ) where

growingPlant :: (Integral a) => a -> a -> a -> a
growingPlant upSpeed downSpeed desiredHeight =
  (+ 1) $
  ceiling $
  fromIntegral (max 0 $ desiredHeight - upSpeed) /
  fromIntegral (upSpeed - downSpeed)
