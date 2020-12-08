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
  ]
