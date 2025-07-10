
module Main where

import GameLogic

showHelp :: IO ()
showHelp = do
    putStrLn "  - 'start': Begin the game"
    putStrLn "  - 'help': Show this help message"
    putStrLn "  - 'exit': Quit the game"

main :: IO ()
main = do
    putStr "\nEnter a command (type 'help' for a list of commands): "
    input <- getLine

    case input of
        "start" -> do
            startGame
            main
        "help" -> do
            showHelp
            main
        "exit" -> do
            putStrLn "Exiting the game. Goodbye!"
        _ -> do
            putStrLn "Invalid command."
            main
