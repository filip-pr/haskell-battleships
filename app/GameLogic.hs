
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
            let orientation = if rawOrientation == "H" then Horizontal else Vertical
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

-- | Configuration of ships for the game
shipConfiguration :: [ShipType]
shipConfiguration = [Carrier, Battleship, Submarine, Destroyer, PatrolBoat, PatrolBoat]

-- | Starts a new Battleships game
startGame :: IO ()
startGame = do
    playerBoard <- choosePlayerBoard
    opponentBoard <- placeOpponentShips emptyBoard shipConfiguration
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
