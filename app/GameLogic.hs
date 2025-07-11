
module GameLogic where

import Data.Char (toLower)
import Text.Read (readMaybe)
import System.Random (randomRIO)

import GameUtils

-- | Places ships randomly on the board
placeShipsRandomly :: GameBoard -> [ShipType] -> IO GameBoard
placeShipsRandomly board [] = return board
placeShipsRandomly board shipTypes@(shipType : rest) = do
    x <- randomRIO (0 :: Int, 9 :: Int)
    y <- randomRIO (0 :: Int, 9 :: Int)
    orientationInt <- randomRIO (0 :: Int, 1 :: Int)
    let orientation = if orientationInt == 0 then Horizontal else Vertical
    let newShip = createShip shipType (x, y) orientation
    let newBoard = addShip board newShip
    case newBoard of
        Left nb -> placeShipsRandomly nb rest -- Valid placement, continue with the next ship
        Right _ -> placeShipsRandomly board shipTypes -- Invalid placement, try again with the same ship

-- | Places the opponent's ships on the board randomly
placeOpponentShips :: GameBoard -> [ShipType] -> IO GameBoard
placeOpponentShips = placeShipsRandomly

-- | Gets the player's ships by prompting for input
getPlayersShips :: GameBoard -> [ShipType] -> IO GameBoard
getPlayersShips board [] = return board
getPlayersShips board shipTypes@(shipType : rest) = do
    putStr $ "\nPlace your " ++ show shipType ++ ": "
    rawInput <- getLine
    let input = map toLower rawInput
    let values = words input
    -- Validate input length
    if length values /= 3
        then do
            if input == "random"
                then do
                    newBoard <- placeShipsRandomly board [shipType]
                    showAndConfirmPlacement newBoard
                else do
                    putStrLn "Invalid input. Please enter positioning in the format '<row char> <col number> <H/V>', or enter 'random' for random ship placement."
                    getPlayersShips board shipTypes
        else validateAndPlace values
    where
        -- Validate and place the ship based on user input
        validateAndPlace values = do
            let rawY = case values !! 0 of
                    [y] -> Just y
                    _   -> Nothing
            let rawX = readMaybe (values !! 1) :: Maybe Int
            let rawOrientation = values !! 2

            case (rawY, rawX, rawOrientation) of
                (Nothing, _, _) -> invalidCoordinates
                (_, Nothing, _) -> invalidCoordinates
                (Just y, Just x, orientation)
                    | y `notElem` (['a'..'j']) -> invalidCoordinates
                    | x `notElem` [1..10] -> invalidCoordinates
                    | orientation `notElem` ["h", "v"] -> invalidOrientation
                    | otherwise -> tryPlaceShip (x - 1) (fromEnum y - fromEnum 'a') orientation

        -- Handle invalid coordinates
        invalidCoordinates = do
            putStrLn "Invalid coordinates. Please enter A-J for row and 1-10 for column, ie. 'A 3 <orientation>'."
            getPlayersShips board shipTypes

        -- Handle invalid orientation
        invalidOrientation = do
            putStrLn "Invalid orientation. Please enter 'H' for horizontal or 'V' for vertical."
            getPlayersShips board shipTypes

        -- Try to place the ship on the board
        tryPlaceShip x y rawOrientation = do
            let orientation = if rawOrientation == "h" then Horizontal else Vertical
            let newShip = createShip shipType (x, y) orientation
            case addShip board newShip of
                Left nb -> showAndConfirmPlacement nb
                Right errorMsg -> do
                    putStrLn ("Invalid placement. " ++ errorMsg)
                    getPlayersShips board shipTypes

        -- Show the board and confirm placement
        showAndConfirmPlacement newBoard = do
            putStrLn ("\nCurrent board state:\n\n" ++ showBoardInformation newBoard True)
            putStr "Type 'y' to confirm the placement or 'n' to retry: "
            input <- getLine
            case input of
                "y" -> getPlayersShips newBoard rest
                "n" -> getPlayersShips board shipTypes
                _   -> do
                    putStrLn "Invalid input. Please type 'y' to confirm or 'n' to retry."
                    showAndConfirmPlacement newBoard


performOpponentAction :: GameBoard -> IO GameBoard
performOpponentAction gameBoard@(GameBoard fields ships) = do
    coordinates <- chooseCoordinates
    case registerHit gameBoard coordinates of
        (Just newBoard, _) -> return newBoard
        (Nothing, errorMsg) ->
            error ("Opponent tried to perform invalid action: " ++ errorMsg) -- This should not happen
    where
        -- Get all the ships that are still not sunk
        remainingShips = getRemainingShips ships
        -- Get list of all coordinates that were a hit (because of our strategy, they will always be part of one ship)
        hitShipFields = filter (\coord -> getFieldState fields coord == Hit) (concatMap (\(Ship _ coords _) -> coords) remainingShips)

        chooseCoordinates = if null hitShipFields
            then do
                -- If no hits, choose a random coordinate
                x <- randomRIO (0 :: Int, 9 :: Int)
                y <- randomRIO (0 :: Int, 9 :: Int)
                if getFieldState fields (x, y) /= Unknown
                    then chooseCoordinates -- Retry if the field is not Unknown
                    else return (x, y) -- Return the random coordinates
            else do
                -- Find the top left hit coordinate
                let (tlX,tlY) = foldr (\(minX, minY) (x, y) -> if minX + minY < x + y then (minX, minY) else (x, y)) (10, 10) hitShipFields
                -- Generate potential placements around the top left hit coordinate (most are invalid, but they get filtered out later)
                let potentialPlacements = filter (\coords -> isInsideBoard [coords]) [(x, y) | x <- [tlX - 3 .. tlX], y <- [tlY - 3 .. tlY]]
                -- Generate a ship of one of the remaining ship types
                shipTypeIndex <- randomRIO (0 :: Int, length remainingShips - 1)
                let Ship shipType _ _ = remainingShips !! shipTypeIndex
                -- Placed the ship randomly in one of the potential placements
                shipPlacementIndex <- randomRIO (0 :: Int, length potentialPlacements - 1)
                let (x, y) = potentialPlacements !! shipPlacementIndex
                -- And oriented randomly
                shipOrientationIndex <- randomRIO (0 :: Int, 1 :: Int)
                let orientation = if shipOrientationIndex == 0 then Horizontal else Vertical
                let (Ship _ shipCoords _) = createShip shipType (x, y) orientation
                -- Check if the guess is valid
                if guessIsValid shipCoords
                    then do
                        -- Filter out coordinates that were already hit (or missed) and those further than 1 distance from any already hit field
                        let guessCandidates = filter (\coord -> getFieldState fields coord == Unknown && distanceToMore [coord] hitShipFields == 1) shipCoords
                        guessCandidateIndex <- randomRIO (0 :: Int, length guessCandidates - 1)
                        return (guessCandidates !! guessCandidateIndex)
                    else chooseCoordinates -- Retry if not valid
                where
                    -- Guess is valid if it is inside the board
                    -- the guessed ship type is larger than current number of hits
                    -- none of the guessed coordinates are already a miss
                    -- and all of the hit coordinates are part of the guessed ship
                    guessIsValid coords
                        | length coords <= length hitShipFields = False
                        | not (isInsideBoard coords) = False
                        | any (\coord -> getFieldState fields coord == Miss) coords = False
                        | any (\coord -> coord `notElem` coords) hitShipFields = False
                        | otherwise = True

-- | Performs the player's action by prompting for coordinates and registering a hit
performPlayerAction :: GameBoard -> IO GameBoard
performPlayerAction board = do
    putStr "Enter your attack coordinates: "
    rawInput <- getLine
    let input = map toLower rawInput
    let values = words input
    -- Validate input length
    if length values /= 2
        then do
            putStrLn "Invalid input. Please enter coordinates in the format '<row char> <col number>'."
            performPlayerAction board
        else do
            -- Validate and parse the coordinates
            let rawY = case values !! 0 of
                    [y] -> Just y
                    _   -> Nothing
            let rawX = readMaybe (values !! 1) :: Maybe Int
            case (rawY, rawX) of
                (Nothing, _) -> invalidCoordinates
                (_, Nothing) -> invalidCoordinates
                (Just y, Just x)
                    | y `notElem` (['a'..'j']) -> invalidCoordinates
                    | x `notElem` [1..10] -> invalidCoordinates
                    | otherwise -> tryAttack (x - 1, fromEnum y - fromEnum 'a')
            where
                -- Handle invalid coordinates
                invalidCoordinates = do
                    putStrLn "Invalid coordinates. Please enter A-J for row and 1-10 for column, ie. 'A 3'."
                    performPlayerAction board

                -- Try to register the hit at the given coordinates, print the result and return the new board
                tryAttack (x, y) = do
                    case registerHit board (x, y) of
                        (Just newBoard, message) -> do
                            putStrLn (message ++ "\n")
                            return newBoard
                        (Nothing, errorMsg) -> do
                            putStrLn ("Invalid attack: " ++ errorMsg)
                            performPlayerAction board


-- | Configuration of ships for the game
shipConfiguration :: [ShipType]
shipConfiguration = [Carrier, Battleship, Submarine, Destroyer, PatrolBoat, PatrolBoat]

-- | Starts a new Battleships game
startGame :: IO ()
startGame = do
    opponentBoard <- placeOpponentShips emptyBoard shipConfiguration
    playerBoard <- choosePlayerBoard
    putStrLn (showPlayerBoard playerBoard)
    return ()

    where
        -- Let the player choose the configuration of their board (manual or random)
        choosePlayerBoard = do
            putStr "\nDo you want to place your ships manually 'm' or randomly 'r' ?: "
            rawInput <- getLine
            let input = map toLower rawInput
            case input of
                "m" -> getPlayersShips emptyBoard shipConfiguration
                "r" -> placeShipsRandomly emptyBoard shipConfiguration
                _   -> do
                    putStrLn "Invalid input. Please type 'm' for manual placement or 'r' for random placement."
                    choosePlayerBoard
