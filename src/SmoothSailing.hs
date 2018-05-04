module SmoothSailing
  ( allLongestStrings
  ) where

allLongestStrings :: Foldable t => [t a] -> [t a]
allLongestStrings inputArray = filter ((== maxLen) . length) inputArray
  where
    maxLen = maximum $ map length inputArray

commonCharacterCount :: (Foldable t, Eq p, Num b) => [p] -> t p -> b
commonCharacterCount s1 s2 =
  snd $
  foldl
    (\(p, t) c ->
       case removeFrom c p of
         Just l -> (l, t + 1)
         Nothing -> (p, t))
    (s1, 0)
    s2

removeFrom :: Eq p => p -> [p] -> Maybe [p]
removeFrom c s = f [] s
  where
    f _ [] = Nothing
    f xs (y:ys) =
      if y == c
        then Just (xs ++ ys)
        else f (y : xs) ys

isLucky :: Show p => p -> Bool
isLucky n = even lenN && f firstHalf == f secondHalf
  where
    strN = show n
    lenN = length strN
    (firstHalf, secondHalf) =
      (take (lenN `div` 2) strN, drop (lenN `div` 2) strN)
    f = sum . map digitToInt

digitToInt :: Num p => Char -> p
digitToInt '0' = 0
digitToInt '1' = 1
digitToInt '2' = 2
digitToInt '3' = 3
digitToInt '4' = 4
digitToInt '5' = 5
digitToInt '6' = 6
digitToInt '7' = 7
digitToInt '8' = 8
digitToInt '9' = 9
digitToInt c = error (c : " is not a digit")
