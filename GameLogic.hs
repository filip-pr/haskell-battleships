module GameLogic where

import Data.Maybe (fromJust)

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

data Ship = Ship ShipType [Coordinates] Int
    deriving (Show, Eq)

data GameBoard = GameBoard [[Field]] [Ship]
    deriving (Show, Eq)

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

distance :: Coordinates -> Coordinates -> Int
distance (x1, y1) (x2, y2) = max (abs (x1 - x2)) (abs (y1 - y2))

collidesWith :: Ship -> [Ship] -> Bool
collidesWith (Ship _ coords _) ships
    | null coords || null ships = False
    | otherwise = minimum [distance coord otherCoord | coord <- coords, (Ship _ otherCoords _) <- ships, otherCoord <- otherCoords] <= 1

addShip :: GameBoard -> Ship -> Either GameBoard String
addShip (GameBoard fields ships) ship@(Ship _ coords _)
    | collidesWith ship ships = Right "Ships must be at least one square apart from each other."
    | not (isInsideBoard coords) = Right "Ship must be place fully inside the board."
    | otherwise = Left (GameBoard fields (ship : ships))

getShip :: Coordinates -> [Ship] -> Maybe Ship
getShip _ [] = Nothing
getShip coords (ship@(Ship _ shipCoords _) : rest)
    | coords `elem` shipCoords = Just ship
    | otherwise = getShip coords rest

isShip :: Coordinates -> [Ship] -> Bool
isShip coords ships = case getShip coords ships of
    Just _ -> True
    Nothing -> False

replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal values
    | n < 0 || n >= length values = values
    | otherwise = take n values ++ [newVal] ++ drop (n + 1) values

replaceField :: Field -> Coordinates -> [[Field]] -> [[Field]]
replaceField newField (x, y) fields
    | not (isInsideBoard [(x, y)]) = fields
    | otherwise = replaceNth y (replaceNth x newField (fields !! y)) fields

registerHit :: GameBoard -> Coordinates -> (Maybe GameBoard, String)
registerHit (GameBoard fields ships) coords@(x, y)
    | not (isInsideBoard [coords]) = (Nothing, "Coordinates are out of bounds!")
    | fields !! y !! x /= Unknown = (Nothing, "Field already hit!")
    | otherwise = (Just newGameBoard, responseMessage) where
        hitShip = getShip coords ships
        newField = case hitShip of
            Just (Ship _ _ remainingHits) ->
                if remainingHits <= 1
                then Sunken
                else Hit
            Nothing -> Miss
        shipType = case hitShip of
            Just (Ship st _ _) -> st
            Nothing -> Carrier -- Default to Carrier if no ship is hit
        shipCoords = case hitShip of
            Just (Ship _ coords _) -> coords
            Nothing -> []
        responseMessage = case newField of
            Sunken -> "Hit!\n" ++ showShip (fromJust hitShip)  ++ "sunken!"
            Hit -> "Hit!"
            Miss -> "Miss!"
        newFields = case newField of
            Sunken -> foldr (replaceField Sunken) fields shipCoords
            _ -> replaceField newField coords fields
        newShips = case hitShip of
            Just ship -> map (\s@(Ship st sc rh) -> if isShip coords [s] then Ship st sc (rh-1) else s) ships
            Nothing -> ships
        newGameBoard = GameBoard newFields newShips

getRemainingShips :: [Ship] -> [Ship]
getRemainingShips [] = []
getRemainingShips (ship@(Ship _ _ remainingHits) : rest)
    | remainingHits <= 0 = getRemainingShips rest
    | otherwise = ship : getRemainingShips rest

shipChar = '#'
unknownFieldChar = ' '
missFieldChar = 'O'

hitFieldChar = '@'
sunkenFieldChar = 'S'

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


showBoardInformation :: GameBoard -> Bool -> String
showBoardInformation gameboard@(GameBoard fields ships) isYour
    | isYour = "Your information:\n\n" ++ "Remaining ships:\n\n" ++ concatMap (\ship -> showShip ship ++ "\n\n") (getRemainingShips ships) ++ showBoard gameboard isYour
    | otherwise = "Opponent's information:\n\n" ++ "Remaining ships:\n\n" ++ concatMap (\ship -> showShip ship ++ "\n\n") (getRemainingShips ships) ++ showBoard gameboard isYour


showBoard :: GameBoard -> Bool -> String
showBoard (GameBoard fields ships) isYour =
    "GameBoard:\n\n"
    ++ "     0   1   2   3   4   5   6   7   8   9  \n"
    ++ "   +---+---+---+---+---+---+---+---+---+---+\n"
    ++ concatMap showFieldRow (zip3 ['A'..] fields [[isYour && isShip (row, col) ships | col <- [0 .. 9]] | row <- [0 .. 9]])
