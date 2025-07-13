module Main where

import Test.Tasty

import GameUtilsTest
import GameLogicTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Battleships Tests" [gameUtilsTests, gameLogicTests]
