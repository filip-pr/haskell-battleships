
module Main where

import System.IO

import GameLogic

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    startGame
