module Main where

import Data.Maybe
import EdgeOfTheOcean
import ExploringTheWaters
import Intro
import IslandOfKnowledge
import SmoothSailing
import Text.Read

superUncurry :: (t1 -> t2 -> t3 -> t4 -> t5) -> ((t1, t2), (t3, t4)) -> t5
superUncurry f = \((a, b), (c, d)) -> f a b c d

toInputReader :: (Read a2, Show a1) => (a2 -> a1) -> String -> Maybe String
toInputReader f = (>>= Just . show . f) . readMaybe

problems =
  [ toInputReader $ uncurry add
  , toInputReader $ centuryFromYear
  , toInputReader $ checkPalindrome
  , toInputReader $ adjacentElementsProduct
  , toInputReader $ shapeArea
  , toInputReader $ makeArrayConsecutive2
  , toInputReader $ almostIncreasingSequence
  , toInputReader $ matrixElementsSum
  , toInputReader $ allLongestStrings
  , toInputReader $ uncurry commonCharacterCount
  , toInputReader $ isLucky
  , toInputReader $ sortByHeight
  , toInputReader $ reverseParentheses
  , toInputReader $ alternatingSums
  , toInputReader $ addBorder
  , toInputReader $ uncurry areSimilar
  , toInputReader $ arrayChange
  , toInputReader $ palindromeRearranging
  , toInputReader $ superUncurry areEquallyStrong
  , toInputReader $ arrayMaximalAdjacentDifference
  , toInputReader $ isIPv4Address
  , toInputReader $ avoidObstacles
  ]

main :: IO ()
main = do
  pbl <- promptInt "Choose a problem:"
  if pbl - 1 >= 0 && pbl <= length problems
    then promptLine "Insert your input:" >>=
         putStrLn .
         fromMaybe "Could not parse input value" . (problems !! (pbl - 1))
    else return ()
  main

always :: p1 -> p2 -> p1
always a b = a

retry :: IO b -> IO b
retry a = putStrLn "Could not parse input:" >>= always a

promptLine :: String -> IO String
promptLine prompt = putStrLn prompt >>= always getLine

promptInt :: String -> IO Int
promptInt prompt =
  promptLine prompt >>= \ln ->
    case readMaybe ln of
      Just x -> return x
      Nothing -> retry $ promptInt prompt
