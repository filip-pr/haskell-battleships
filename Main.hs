
module Main where

import GameBoard

main :: IO ()
main = do
    putStrLn (showBoard testBoard True)
    putStrLn (showBoard testBoard False)
