module GameLogicTest where

import Test.Tasty
import Test.Tasty.QuickCheck
import System.Timeout (timeout)
import System.IO.Silently (capture)

import GameUtils
import GameLogic

testTimeout :: Int
testTimeout = 1000000  -- 1 second

gameLogicTests :: TestTree
gameLogicTests = testGroup "GameLogic Tests"
    [ testGroup "placeShipsRandomly Tests"
        [ testProperty "ship placement terminates with shipConfiguration" $
            withMaxSuccess 100 $ ioProperty $ do
                maybeBoard <- timeout testTimeout (placeShipsRandomly emptyBoard shipConfiguration)
                return $ case maybeBoard of
                    Nothing -> False
                    Just _  -> True
        , testProperty "all ships get placed" $
            withMaxSuccess 100 $ ioProperty $ do
                maybeBoard <- timeout testTimeout (placeShipsRandomly emptyBoard shipConfiguration)
                return $ case maybeBoard of
                    Nothing -> False
                    Just (GameBoard _ ships) ->
                        let placedShips = map (\(Ship t _ _) -> t) ships
                        in all (\ship -> ship `elem` placedShips) shipConfiguration &&
                            (length placedShips == length shipConfiguration)
        ]

    , testGroup "performOpponentAction Tests"
        [
            testProperty "opponent action selection terminates" $
                withMaxSuccess 10 $ ioProperty $ do
                    maybeBoard <- timeout testTimeout (placeShipsRandomly emptyBoard shipConfiguration)
                    case maybeBoard of
                        Nothing -> return False
                        Just board  -> do
                            let loop 0 _ = return True
                                loop n currentBoard = do
                                    (_, maybeNewBoard) <- capture $ timeout testTimeout (performOpponentAction currentBoard)
                                    case maybeNewBoard of
                                        Nothing -> return False
                                        Just newBoard -> loop (n - 1) newBoard
                            loop (100 :: Int) board
        ]
    ]
