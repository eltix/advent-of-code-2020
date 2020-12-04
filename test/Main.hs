module Main where

import           BasicPrelude
import           Test.Tasty
import           Test.Tasty.HUnit

import qualified Day01
import qualified Day02
import qualified Day03

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testCase "Day 1" $ Day01.computeSolutions >>= (@?= (Just 719796, Just 144554112))
  , testCase "Day 2" $ Day02.computeSolutions >>= (@?= (454, 649))
  , testCase "Day 3" $ Day03.computeSolutions >>= (@?= (278, 9709761600))
  ]
