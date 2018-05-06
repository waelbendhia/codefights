module Main where

import EdgeOfTheOcean
import ExploringTheWaters
import Intro
import SmoothSailing
import Text.Read

problems :: [String -> String]
problems =
  [ show . uncurry add . read
  , show . centuryFromYear . read
  , show . checkPalindrome
  , show . adjacentElementsProduct . read
  , show . shapeArea . read
  , show . makeArrayConsecutive2 . read
  , show . almostIncreasingSequence . read
  , show . matrixElementsSum . read
  , show . allLongestStrings . read
  , show . uncurry commonCharacterCount . read
  , show . isLucky . read
  , show . sortByHeight . read
  , show . reverseParentheses . read
  , show . alternatingSums . read
  , show . addBorder . read
  , show . uncurry areSimilar . read
  , show . arrayChange . read
  ]

main :: IO ()
main = do
  pbl <- promptInt "Choose a problem:"
  -- TODO: Check index in bounds
  input <- promptLine "Insert your input:"
  -- TODO: Handle read no parse errors
  putStrLn $ problems !! (pbl - 1) $ input
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
