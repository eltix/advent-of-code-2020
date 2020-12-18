module Main where

import           BasicPrelude
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Day01
import qualified Day02
import qualified Day03
import qualified Day04
import qualified Day05
import qualified Day06
import qualified Day07
import qualified Day08
import qualified Day09
import qualified Day10
import qualified Day11
import qualified Day12
import qualified Day13
import qualified Day14
import qualified Day15
import qualified Day16

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testCase "Day 1" $ Day01.computeSolutions >>= (@?= (Just 719796, Just 144554112))
  , testCase "Day 2" $ Day02.computeSolutions >>= (@?= (454, 649))
  , testCase "Day 3" $ Day03.computeSolutions >>= (@?= (278, 9709761600))
  , testCase "Day 4" $ Day04.computeSolutions >>= (@?= (208, 167))
  , testCase "Day 5" $ Day05.computeSolutions >>= (@?= (822, Just 705))
  , testCase "Day 6" $ Day06.computeSolutions >>= (@?= (6748,3445))
  , testCase "Day 7" $ Day07.computeSolutions >>= (@?= (252, 35487))
  , testCase "Day 8" $ Day08.computeSolutions >>= (@?= (Just 1317, Just 1033))
  , testCase "Day 9" $ Day09.computeSolutions >>= (@?= (Just 69316178, Just 9351526))
  , testCase "Day 10" $ Day10.computeSolutions >>= (@?= (1998,347250213298688))
  , testCase "Day 11" $ Day11.computeSolutions >>= (@?= (2483, 2285))
  , testCase "Day 12" $ Day12.computeSolutions >>= (@?= (1010, 52742))
  , testCase "Day 13" $ Day13.computeSolutions >>= (@?= (104,842186186521918))
  , testCase "Day 14" $ Day14.computeSolutions >>= (@?= (14722016054794, 3618217244644))
  , testCase "Day 15" $ Day15.computeSolutions >>= (@?= (0, 0))
  , testCase "Day 16" $ Day16.computeSolutions >>= (@?= (0, 0))
  ]
