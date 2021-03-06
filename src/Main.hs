module Main where

import DarkWilderness
import Data.Maybe
import DivingDeeper
import EdgeOfTheOcean
import EruptionOfLight
import ExploringTheWaters
import Intro
import IslandOfKnowledge
import LandOfLogic
import RainbowOfClarity
import RainsOfReason
import SmoothSailing
import Text.Read
import ThroughTheFog

uncurry3 :: (t1 -> t2 -> t3 -> t4) -> (t1, t2, t3) -> t4
uncurry3 f = \(a, b, c) -> f a b c

uncurry4 :: (t1 -> t2 -> t3 -> t4 -> t5) -> (t1, t2, t3, t4) -> t5
uncurry4 f = \(a, b, c, d) -> f a b c d

uncurry5 :: (t1 -> t2 -> t3 -> t4 -> t5 -> t6) -> (t1, t2, t3, t4, t5) -> t6
uncurry5 f = \(a, b, c, d, e) -> f a b c d e

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
  , toInputReader $ uncurry4 areEquallyStrong
  , toInputReader $ arrayMaximalAdjacentDifference
  , toInputReader $ isIPv4Address
  , toInputReader $ avoidObstacles
  , toInputReader $ boxBlur
  , toInputReader $ minesweeper
  , toInputReader $ uncurry3 arrayReplace
  , toInputReader $ evenDigitsOnly
  , toInputReader $ variableName
  , toInputReader $ alphabeticShift
  , toInputReader $ uncurry chessBoardCellColor
  , toInputReader $ uncurry circleOfNumbers
  , toInputReader $ uncurry3 depositProfit
  , toInputReader $ absoluteValuesSumMinimization
  , toInputReader $ stringsRearrangement
  , toInputReader $ uncurry extractEachKth
  , toInputReader $ firstDigit
  , toInputReader $ differentSymbolsNaive
  , toInputReader $ uncurry arrayMaxConsecutiveSum
  , toInputReader $ uncurry3 growingPlant
  , toInputReader $ uncurry5 knapsackLight
  , toInputReader $ longestDigitsPrefix
  , toInputReader $ digitDegree
  , toInputReader $ uncurry bishopAndPawn
  , toInputReader $ isBeautifulString
  , toInputReader $ findEmailDomain
  , toInputReader $ buildPalindrome
  , toInputReader $ uncurry electionsWinners
  , toInputReader $ isMAC48Address
  , toInputReader $ isDigit
  , toInputReader $ lineEncoding
  , toInputReader $ chessKnight
  , toInputReader $ deleteDigit
  , toInputReader $ longestWord
  , toInputReader $ validTime
  , toInputReader $ sumUpNumbers
  , toInputReader $ differentSquares
  , toInputReader $ digitsProduct
  , toInputReader $ messageFromBinaryCode
  , toInputReader $ spiralNumbers
  , toInputReader $ sudoku
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
