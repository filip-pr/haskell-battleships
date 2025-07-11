module GameUtils where

import Data.Maybe (fromJust)

-- | Type representing board coordinates
type Coordinates = (Int, Int)

-- | Type (enum) representing the (visible) state of a field on a board
data FieldState = Unknown | Miss | Hit | Sunken
    deriving (Show, Eq)

-- | Type (enum) representing the orientation of a ship
data ShipOrientation = Horizontal | Vertical
    deriving (Show, Eq)

-- | Type (enum) representing the type of a ship
data ShipType =
    Carrier | -- 4x2
    Battleship | -- 4x1
    Submarine | -- 2x2
    Destroyer | -- 3x1
    PatrolBoat -- 2x1
    deriving Eq

instance Show ShipType where
    show Carrier = "Carrier (4x2)"
    show Battleship = "Battleship (4x1)"
    show Submarine = "Submarine (2x2)"
    show Destroyer = "Destroyer (3x1)"
    show PatrolBoat = "Patrol Boat (2x1)"

-- | Type representing a ship, with its type, coordinates and remaining hits
data Ship = Ship ShipType [Coordinates] Int
    deriving (Show, Eq)

-- | Type representing a game board as a field state matrix and a list of ships
data GameBoard = GameBoard [[FieldState]] [Ship]
    deriving (Show, Eq)


-- | Creates a new ship of given type, coordinates and orientation
createShip :: ShipType -> Coordinates -> ShipOrientation -> Ship
createShip Carrier (x, y) Horizontal = Ship Carrier [(x, y), (x + 1, y), (x + 2, y), (x + 3, y), (x, y + 1), (x + 1, y + 1), (x + 2, y + 1), (x + 3, y + 1)] 8
createShip Carrier (x, y) Vertical = Ship Carrier [(x, y), (x, y + 1), (x, y + 2), (x, y + 3), (x + 1, y), (x + 1, y + 1), (x + 1, y + 2), (x + 1, y + 3)] 8
createShip Battleship (x, y) Horizontal = Ship Battleship [(x, y), (x + 1, y), (x + 2, y), (x + 3, y)] 4
createShip Battleship (x, y) Vertical = Ship Battleship [(x, y), (x, y + 1), (x, y + 2), (x, y + 3)] 4
createShip Submarine (x, y) _ = Ship Submarine [(x, y), (x + 1, y), (x, y + 1), (x + 1, y + 1)] 4
createShip Destroyer (x, y) Horizontal = Ship Destroyer [(x, y), (x + 1, y), (x + 2, y)] 3
createShip Destroyer (x, y) Vertical = Ship Destroyer [(x, y), (x, y + 1), (x, y + 2)] 3
createShip PatrolBoat (x, y) Horizontal = Ship PatrolBoat [(x, y), (x + 1, y)] 2
createShip PatrolBoat (x, y) Vertical = Ship PatrolBoat [(x, y), (x, y + 1)] 2

-- | Creates a new game board with all fields set to Unknown and no ships
emptyBoard :: GameBoard
emptyBoard = GameBoard [[Unknown | _ <- [1 .. 10 :: Int]] | _ <- [1 .. 10 :: Int]] []

-- | Checks if a list of coordinates is fully inside the board (0 <= x < 10, 0 <= y < 10)
isInsideBoard :: [Coordinates] -> Bool
isInsideBoard [] = True
isInsideBoard ((x, y) : rest) = 0 <= x && x < 10 && 0 <= y && y < 10 && isInsideBoard rest

-- | Calculates the distance between two coordinates, defined as the maximum of the absolute differences in x and y
distance :: Coordinates -> Coordinates -> Int
distance (x1, y1) (x2, y2) = max (abs (x1 - x2)) (abs (y1 - y2))

-- | Calculates the distance from a coordinate to the nearest coordinate in a list
distanceToMore :: [Coordinates] -> [Coordinates] -> Int
distanceToMore _ [] = 0
distanceToMore coords1 coords2 = minimum [distance coord otherCoord | coord <- coords1, otherCoord <- coords2]

-- | Checks if a ship collides (has any 8-way neighboring or shared fields) with any other ship on the board
collidesWith :: Ship -> [Ship] -> Bool
collidesWith (Ship _ coords _) ships
    | null coords || null ships = False
    | otherwise = distanceToMore coords (concatMap (\(Ship _ otherCoords _) -> otherCoords) ships) <= 1

-- | Adds a ship to the game board or returns an error message if it collides with another ship or is not fully inside the board
addShip :: GameBoard -> Ship -> Either GameBoard String
addShip (GameBoard fields ships) ship@(Ship _ coords _)
    | collidesWith ship ships = Right "Ships must be at least one square apart from each other."
    | not (isInsideBoard coords) = Right "Ship must be place fully inside the board."
    | otherwise = Left (GameBoard fields (ship : ships))

-- | Gets the ship at given coordinates, if it exists
getShip :: Coordinates -> [Ship] -> Maybe Ship
getShip _ [] = Nothing
getShip coords (ship@(Ship _ shipCoords _) : rest)
    | coords `elem` shipCoords = Just ship
    | otherwise = getShip coords rest

-- | Checks if a given coordinates is occupied by a ship
isShip :: Coordinates -> [Ship] -> Bool
isShip coords ships = case getShip coords ships of
    Just _ -> True
    Nothing -> False

-- | Replaces the n-th element in a list with a new value
replaceNth :: Int -> a -> [a] -> [a]
replaceNth n newVal values
    | n < 0 || n >= length values = values
    | otherwise = take n values ++ [newVal] ++ drop (n + 1) values

-- | Gets the field state at given coordinates, returning Unknown if the coordinates are out of bounds
getFieldState :: [[FieldState]] -> Coordinates -> FieldState
getFieldState fields (x, y)
    | not (isInsideBoard [(x, y)]) = Unknown
    | otherwise = fields !! y !! x

-- | Replaces the field state at given coordinates with a new field state in the game board
replaceFieldState :: FieldState -> Coordinates -> [[FieldState]] -> [[FieldState]]
replaceFieldState newFieldState (x, y) fields
    | not (isInsideBoard [(x, y)]) = fields
    | otherwise = replaceNth y (replaceNth x newFieldState (fields !! y)) fields

-- | Registers a hit on the game board at given coordinates, updating the field state and ship status
-- and return a response message (and the updated game board if the coordinates are valid)
registerHit :: GameBoard -> Coordinates -> (Maybe GameBoard, String)
registerHit (GameBoard fields ships) coords@(x, y)
    | not (isInsideBoard [coords]) = (Nothing, "Coordinates are out of bounds!")
    | fields !! y !! x /= Unknown = (Nothing, "Coordinates already hit!")
    | otherwise = (Just newGameBoard, responseMessage) where
        hitShip = getShip coords ships
        newFieldState = case hitShip of
            Just (Ship _ _ remainingHits) ->
                if remainingHits <= 1
                then Sunken
                else Hit
            Nothing -> Miss
        shipCoords = case hitShip of
            Just (Ship _ c _) -> c
            Nothing -> []
        responseMessage = case newFieldState of
            Sunken -> "Hit!\n" ++ show (fromJust hitShip)  ++ "sunken!"
            Hit -> "Hit!"
            Miss -> "Miss!"
            Unknown -> error "This shouldn't happen"
        newFieldStates = case newFieldState of
            Sunken -> foldr (replaceFieldState Sunken) fields shipCoords
            _ -> replaceFieldState newFieldState coords fields
        newShips = case hitShip of
            Just _ -> map (\s@(Ship st sc rh) -> if isShip coords [s] then Ship st sc (rh-1) else s) ships
            Nothing -> ships
        newGameBoard = GameBoard newFieldStates newShips

-- | Gets the remaining ships on the board, filtering out those that have no remaining hits (are sunken)
getRemainingShips :: [Ship] -> [Ship]
getRemainingShips [] = []
getRemainingShips (ship@(Ship _ _ remainingHits) : rest)
    | remainingHits <= 0 = getRemainingShips rest
    | otherwise = ship : getRemainingShips rest


-- | Displays the game board information, including remaining ships and field states
-- (with revealed ship positions if the board is the player's)
showBoardInformation :: GameBoard -> Bool -> String
showBoardInformation (GameBoard fields ships) showShips
    = "Board information:\n\n" ++ showRemainingShips ++ showBoard
    where
        -- Characters used for displaying the game board
        shipChar = '#'
        unknownFieldStateChar = ' '
        missFieldStateChar = 'O'
        hitFieldStateChar = 'X'
        sunkenFieldStateChar = 'S'

        -- Displays the ships in a formatted string
        showShip (Ship _ _ remainingHits) | remainingHits < 0 = ""
        showShip (Ship Carrier _ _) =    "-Carrier:     " ++ replicate 4 shipChar ++ "\n" ++ replicate 14 ' ' ++ replicate 4 shipChar
        showShip (Ship Battleship _ _) = "-Battleship:  " ++ replicate 4 shipChar
        showShip (Ship Submarine _ _) =  "-Submarine:   " ++ replicate 2 shipChar ++ "\n" ++ replicate 14 ' ' ++ replicate 2 shipChar
        showShip (Ship Destroyer _ _) =  "-Destroyer:   " ++ replicate 3 shipChar
        showShip (Ship PatrolBoat _ _) = "-Patrol Boat: " ++ replicate 2 shipChar

        -- Displays the remaining ships on the board
        showRemainingShips = "Remaining ships:\n\n" ++ concatMap (\ship -> showShip ship ++ "\n\n") (getRemainingShips ships)

        -- Displays the field state
        showFieldState (field, ship) = " " ++ [fieldChar] ++ " |"
            where
            fieldChar = case field of
                Unknown -> if ship then shipChar else unknownFieldStateChar
                Miss -> missFieldStateChar
                Hit -> hitFieldStateChar
                Sunken -> sunkenFieldStateChar

        -- Displays a row of field states with the row label with ships indicated by a boolean list
        showFieldStateRow (rowLabel, fieldRow, shipRow) =
            " "
            ++ [rowLabel]
            ++ " |"
            ++ concatMap showFieldState (zip fieldRow shipRow)
            ++ "\n"
            ++ "   +---+---+---+---+---+---+---+---+---+---+\n"

        -- Displays the game board with field states and ships, showing ships positions only if the board is the player's
        showBoard =
            "GameBoard\n\n"
            ++ "hint: "
            ++ "Unknown: '" ++ [unknownFieldStateChar] ++ "', "
            ++ (if showShips then ("Ship: '" ++ [shipChar] ++ "', ") else "")
            ++ "Miss: '" ++ [missFieldStateChar] ++ "', "
            ++ "Hit: '" ++ [hitFieldStateChar] ++ "', "
            ++ "Sunken: '" ++ [sunkenFieldStateChar] ++ "'\n\n"
            ++ "     1   2   3   4   5   6   7   8   9   10 \n"
            ++ "   +---+---+---+---+---+---+---+---+---+---+\n"
            ++ concatMap showFieldStateRow (zip3 ['A' .. 'J'] fields [[showShips && isShip (col, row) ships | col <- [0 .. 9 :: Int]] | row <- [0 .. 9 :: Int]])

showPlayerBoard :: GameBoard -> String
showPlayerBoard board = showBoardInformation board True

showOpponentBoard :: GameBoard -> String
showOpponentBoard board = showBoardInformation board False
