

module GameBoard where

import Data.Maybe (fromJust)

shipChar = 'X'

unknownFieldChar = 'O'

missFieldChar = 'M'

hitFieldChar = 'H'

sunkenFieldChar = 'S'

type Coordinates = (Int, Int)

data Field = Unknown | Miss | Hit | Sunken
    deriving (Show, Eq)

data ShipOrientation = Horizontal | Vertical
    deriving (Show, Eq)

data ShipType =
    Carrier | -- 4x2
    Battleship | -- 4x1
    Submarine | -- 2x2
    Destroyer | -- 3x1
    PatrolBoat -- 2x1
    deriving (Show, Eq)

data Ship = Ship
    {
    shipType :: ShipType,
    coords :: [Coordinates],
    remainingHits :: Int
    }
    deriving (Show, Eq)

data GameBoard = GameBoard [[Field]] [Ship]

isNeighbor :: Coordinates -> Coordinates -> Bool
isNeighbor (x1, y1) (x2, y2) = max (abs (x1 - x2)) (abs (y1 - y2)) <= 1

newShip :: ShipType -> Coordinates -> ShipOrientation -> Ship
newShip Carrier (x, y) Horizontal = Ship Carrier [(x, y), (x + 1, y), (x + 2, y), (x + 3, y), (x, y + 1), (x + 1, y + 1), (x + 2, y + 1), (x + 3, y + 1)] 8
newShip Carrier (x, y) Vertical = Ship Carrier [(x, y), (x, y + 1), (x, y + 2), (x, y + 3), (x + 1, y), (x + 1, y + 1), (x + 1, y + 2), (x + 1, y + 3)] 8
newShip Battleship (x, y) Horizontal = Ship Battleship [(x, y), (x + 1, y), (x + 2, y), (x + 3, y)] 4
newShip Battleship (x, y) Vertical = Ship Battleship [(x, y), (x, y + 1), (x, y + 2), (x, y + 3)] 4
newShip Submarine (x, y) _ = Ship Submarine [(x, y), (x + 1, y), (x, y + 1), (x + 1, y + 1)] 4
newShip Destroyer (x, y) Horizontal = Ship Destroyer [(x, y), (x + 1, y), (x + 2, y)] 3
newShip Destroyer (x, y) Vertical = Ship Destroyer [(x, y), (x, y + 1), (x, y + 2)] 3
newShip PatrolBoat (x, y) Horizontal = Ship PatrolBoat [(x, y), (x + 1, y)] 2
newShip PatrolBoat (x, y) Vertical = Ship PatrolBoat [(x, y), (x, y + 1)] 2

newBoard :: GameBoard
newBoard = GameBoard [[Unknown | _ <- [1 .. 10]] | _ <- [1 .. 10]] []

isInsideBoard :: [Coordinates] -> Bool
isInsideBoard [] = True
isInsideBoard ((x, y) : rest) = 0 <= x && x < 10 && 0 <= y && y < 10 && isInsideBoard rest

isDisjunct :: (Eq a) => [a] -> [a] -> Bool
isDisjunct [] _ = True
isDisjunct (element:rest) list = notElem element list && isDisjunct rest list

collidesWith :: Ship -> [Ship] -> Bool
collidesWith (Ship _ [] _) _ = False
collidesWith (Ship _ coords _) ships = not (isDisjunct coords (concatMap (\(Ship _ coordinates _) -> coordinates) ships))

addShip :: GameBoard -> Ship -> Maybe GameBoard
addShip (GameBoard fields ships) ship@(Ship _ coords _)
    | not (collidesWith ship ships) && isInsideBoard coords = Just (GameBoard fields (ship:ships))
    | otherwise = Nothing

showShip :: Ship -> String
showShip (Ship _ _ remainingHits) | remainingHits < 0 = ""
showShip (Ship Carrier _ remainingHits) = "-Carrier:   " ++ replicate 4 shipChar ++ "\n" ++ replicate 12 ' ' ++ replicate 4 shipChar
showShip (Ship Battleship _ remainingHits) = "-Battleship:" ++ replicate 4 shipChar
showShip (Ship Submarine _ remainingHits) = "-Submarine: " ++ replicate 2 shipChar ++ "\n" ++ replicate 12 ' ' ++ replicate 2 shipChar
showShip (Ship Destroyer _ remainingHits) = "-Destroyer: " ++ replicate 3 shipChar
showShip (Ship PatrolBoat _ remainingHits) = "-Patrol Boat:" ++ replicate 2 shipChar

showFieldCell :: (Field, Bool) -> String
showFieldCell (field, ship) = " " ++ [showChar] ++ " |"
    where
    fieldChar = case field of
        Unknown -> unknownFieldChar
        Miss -> missFieldChar
        Hit -> hitFieldChar
        Sunken -> sunkenFieldChar
    showChar = if ship then shipChar else fieldChar

showFieldRow :: (Char, [Field], [Bool]) -> String
showFieldRow (rowLabel, fieldRow, ships) =
    " "
    ++ [rowLabel]
    ++ " |"
    ++ concatMap showFieldCell (zip fieldRow ships)
    ++ "\n"
    ++ "   +---+---+---+---+---+---+---+---+---+---+\n"


showBoard :: GameBoard -> Bool -> String
showBoard gameboard@(GameBoard fields ships) isYour
    | isYour = "Your information:\n\n" ++ "Remaining ships:\n\n" ++ concatMap (\ship -> showShip ship ++ "\n\n") ships ++ _showBoard gameboard isYour
    | otherwise = "Opponent's information:\n\n" ++ "Remaining ships:\n\n" ++ concatMap (\ship -> showShip ship ++ "\n\n") ships ++ _showBoard gameboard isYour


isShip :: Coordinates -> [Ship] -> Bool
isShip _ [] = False
isShip coords (ship@(Ship _ shipCoords _):rest) =
    elem coords shipCoords || isShip coords rest

_showBoard :: GameBoard -> Bool -> String
_showBoard (GameBoard fields ships) isYour =
    "GameBoard:\n\n"
    ++ "     0   1   2   3   4   5   6   7   8   9  \n"
    ++ "   +---+---+---+---+---+---+---+---+---+---+\n"
    ++ concatMap showFieldRow (zip3 ['A'..] fields [[isYour && isShip (row, col) ships | col <- [0 .. 9]] | row <- [0 .. 9]])



testBoard = fromJust (addShip newBoard (newShip Carrier (0, 0) Vertical))
