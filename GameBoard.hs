module GameBoard where

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
addShip (GameBoard fields ships) ship
    | not (collidesWith ship ships) = Just (GameBoard fields (ship:ships))
    | otherwise = Nothing
