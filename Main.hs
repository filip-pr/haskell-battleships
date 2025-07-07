
module Main where

import Data.Either (fromLeft)

import GameLogic



testBoard = fromLeft newBoard (addShip newBoard (newShip Carrier (3, 2) Horizontal))

main :: IO ()
main = do
    putStrLn (showBoard testBoard True)
    putStrLn (showBoard testBoard False)
