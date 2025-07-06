
module Main where

import Data.Maybe (fromJust)

import GameBoard


testBoard = fromJust (addShip (fromJust (addShip newBoard (newShip Carrier (0, 0) Vertical))) (newShip Carrier (3, 0) Vertical))

main :: IO ()
main = do
    putStrLn (showBoard testBoard True)
    putStrLn (showBoard testBoard False)
