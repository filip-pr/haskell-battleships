{-# LANGUAGE ScopedTypeVariables #-}

module GameUtilsTest where

import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Control.Monad (liftM2, liftM3)

import GameUtils

-- | Newtype wrappers to avoid orphan instances
newtype TestFieldState = TestFieldState FieldState deriving (Eq, Show)
newtype TestShipType = TestShipType ShipType deriving (Eq, Show)
newtype TestShipOrientation = TestShipOrientation ShipOrientation deriving (Eq, Show)
newtype TestShip = TestShip Ship deriving (Eq, Show)
newtype TestGameBoard = TestGameBoard GameBoard deriving (Eq, Show)

-- | Arbitrary instance for TestFieldState
instance Arbitrary TestFieldState where
    arbitrary = TestFieldState <$> elements [Unknown, Miss, Hit, Sunken]

-- | Arbitrary instance for TestShipType
instance Arbitrary TestShipType where
    arbitrary = TestShipType <$> elements [Carrier, Battleship, Submarine, Destroyer, PatrolBoat]

-- | Arbitrary instance for TestShipOrientation
instance Arbitrary TestShipOrientation where
    arbitrary = TestShipOrientation <$> elements [Horizontal, Vertical]

-- | Arbitrary instance for TestShip (with valid coordinates)
instance Arbitrary TestShip where
    arbitrary = do
        TestShipType shipType <- arbitrary
        -- The ship position will be in the board, but some of its coordinates may not
        -- So this should generate both valid and invalid ship placements (but mostly valid)
        position <- (,) <$> (choose (0, 9)) <*> (choose (0, 9))
        TestShipOrientation orientation <- arbitrary
        let (Ship _ coords rh) = createShip shipType position orientation
        damage <- choose (0, rh)
        return $ TestShip (Ship shipType coords damage)

-- | Arbitrary instance for TestGameBoard
instance Arbitrary TestGameBoard where
    arbitrary = do
        fields <- vectorOf 10 (vectorOf 10 ((\(TestFieldState fs) -> fs) <$> arbitrary))
        ships <- listOf ((\(TestShip ship) -> ship) <$> arbitrary)
        return $ TestGameBoard (GameBoard fields ships)

-- | Generate valid board coordinates (0-9, 0-9)
validCoordinates :: Gen Coordinates
validCoordinates = (,) <$> choose (0, 9) <*> choose (0, 9)

-- | Generate invalid board coordinates (outside 0-9 range)
invalidCoordinates :: Gen Coordinates
invalidCoordinates = oneof [
    (,) <$> choose (-100, -1) <*> arbitrary,
    (,) <$> choose (10, 100) <*> arbitrary,
    (,) <$> arbitrary <*> choose (-100, -1),
    (,) <$> arbitrary <*> choose (10, 100)
    ]

-- | Generate a list of only valid coordinates
validCoordinatesList :: Gen [Coordinates]
validCoordinatesList = listOf validCoordinates

-- | Generate a list containing invalid coordinates
invalidCoordinatesList :: Gen [Coordinates]
invalidCoordinatesList = do
    valid <- listOf validCoordinates
    invalid <- listOf1 invalidCoordinates
    shuffle (valid ++ invalid)

-- | Generate a 10x10 field state matrix
fieldStateMatrix :: Gen [[FieldState]]
fieldStateMatrix = vectorOf 10 (vectorOf 10 ((\(TestFieldState fs) -> fs) <$> arbitrary))

gameUtilsTests :: TestTree
gameUtilsTests = testGroup "GameUtils Tests"
    [ testGroup "isInsideBoard Tests"
        [ testProperty "empty list is inside board" $
            isInsideBoard []
        , testProperty "valid coordinates are inside board" $
            forAll validCoordinatesList $
                \coords -> isInsideBoard coords
        , testProperty "invalid coordinates are outside board" $
            forAll invalidCoordinatesList $
                \coords -> not (isInsideBoard coords)
        ]

    , testGroup "distance Tests"
        [ testProperty "distance is symmetric" $
            \c1 c2 -> distance c1 c2 == distance c2 c1
        , testProperty "distance to self is 0" $
            \c -> distance c c == 0
        , testProperty "distance is non-negative" $
            \c1 c2 -> distance c1 c2 >= 0
        , testProperty "distance satisfies triangle inequality" $
            \c1 c2 c3 -> distance c1 c3 <= distance c1 c2 + distance c2 c3
        , testCase "distance between (0,0) and (9,6) is 9" $
            distance (0, 0) (9, 6) @?= 9
        , testCase "distance between (4,5) and (6,7) is 2" $
            distance (4, 5) (6, 7) @?= 2
        ]

    , testGroup "distanceToMore Tests"
        [ testProperty "distance to empty list fails" $
            \coords -> expectFailure (distanceToMore coords [] == 0)
        , testProperty "distance from empty list fails" $
            \coords -> expectFailure (distanceToMore [] coords == 0)
        , testProperty "distance is symmetric" $
            \coords1 coords2 -> not (null coords1 || null coords2) ==>
                distanceToMore coords1 coords2 == distanceToMore coords2 coords1
        , testProperty "behaves like distance" $
            \coords coord -> not (null coords) ==>
                distanceToMore coords [coord] == minimum [distance c coord | c <- coords]
        ]

    , testGroup "collidesWith Tests"
        [ testProperty "ship doesn't collide with empty ship list" $
            \(TestShip ship) -> not (collidesWith ship [])
        , testProperty "ship collides with itself" $
            \(TestShip ship) -> collidesWith ship [ship]
        , testProperty "ships at distance > 1 don't collide" $
            \(TestShip ship1@(Ship _ coords1 _), TestShip ship2@(Ship _ coords2 _)) ->
                not (null coords1 || null coords2) &&
                distanceToMore coords1 coords2 > 1 ==>
                    not (collidesWith ship1 [ship2])
        ]

    , testGroup "addShip Tests"
        [ testProperty "adding ship with invalid coordinates returns error" $
            forAll (liftM2 (,) arbitrary invalidCoordinatesList) $
                \(TestGameBoard gameBoard, coords) ->
                    case addShip gameBoard (Ship Destroyer coords 3) of
                        Right _ -> True
                        Left _ -> False
        , testProperty "adding destroyer at (3, 2) vertically to empty board is valid" $
            case addShip emptyBoard (createShip Destroyer (3, 2) Vertical) of
                Left _ -> True
                Right _ -> False
        , testProperty "adding patrol boat at (6, 4) horizontally to empty board is valid" $
            case addShip emptyBoard (createShip PatrolBoat (6, 4) Horizontal) of
                Left _ -> True
                Right _ -> False
        , testProperty "adding submarine at (9, 8) to empty board is not valid" $
            case addShip emptyBoard (createShip Submarine (9, 8) Horizontal) of
                Left _ -> False
                Right _ -> True
        , testProperty "adding battleship at (6, 4) horizontally to board with (8, 3) submarine is not valid" $
            case (addShip (GameBoard [] [createShip Submarine (8, 3) Horizontal]) (createShip Battleship (6, 4) Horizontal)) of
                Left _ -> False
                Right _ -> True
        ]

    , testGroup "getShip Tests"
        [ testProperty "no ship found in empty ship list" $
            \coords -> getShip coords [] == Nothing
        , testProperty "ship found if coordinates match" $
            \coords (TestShipType shipType) hits ->
                getShip coords [Ship shipType [coords] hits] == Just (Ship shipType [coords] hits)
        , testProperty "ship not found if coordinates don't match" $
            \coords coordsList (TestShipType shipType) hits ->
                coords `notElem` coordsList ==>
                getShip coords [Ship shipType coordsList hits] == Nothing
        ]

    , testGroup "isShip Tests"
        [ testProperty "no ship at coordinates in empty list" $
            \coords -> not (isShip coords [])
        , testProperty "ship exists if getShip returns Just" $
            \coords (TestShip ship) ->
                isShip coords [ship] == (getShip coords [ship] /= Nothing)
        ]

    , testGroup "replaceNth Tests"
        [ testProperty "replacing negative index doesn't change list" $
            \n newVal (list :: [Int]) -> n < 0 ==> replaceNth n newVal list == list
        , testProperty "replacing index >= length doesn't change list" $
            \n newVal (list :: [Int]) -> n >= length list ==> replaceNth n newVal list == list
        , testProperty "replacing valid index changes the element" $
            \n newVal (list :: [Int]) -> 0 <= n && n < length list ==>
                (replaceNth n newVal list) !! n == newVal
        , testProperty "replacing doesn't change the rest of the list" $
            \n newVal (list :: [Int]) -> 0 <= n && n < length list ==>
                take n (replaceNth n newVal list) == take n list &&
                drop (n + 1) (replaceNth n newVal list) == drop (n + 1) list
        ]

    , testGroup "getFieldState Tests"
        [ testProperty "out of bounds coordinates return Unknown" $
            forAll (liftM2 (,) fieldStateMatrix invalidCoordinates) $
                \(fields, coords) -> getFieldState fields coords == Unknown
        , testProperty "valid coordinates return correct field state" $
            forAll (liftM2 (,) fieldStateMatrix validCoordinates) $
                \(fields, coords@(x, y)) -> getFieldState fields coords == (fields !! y !! x)
        ]

    , testGroup "replaceFieldState Tests"
        [ testProperty "replacing out of bounds coordinates doesn't change fields" $
            forAll (liftM3 (,,) arbitrary invalidCoordinates fieldStateMatrix) $
                \(TestFieldState newState, coords, fields) -> replaceFieldState newState coords fields == fields
        , testProperty "replacing valid coordinates changes the field" $
            forAll (liftM3 (,,) arbitrary validCoordinates fieldStateMatrix) $
                \(TestFieldState newState, coords@(x, y), fields) -> ((replaceFieldState newState coords fields) !! y !! x) == newState
        , testProperty "replacing doesn't change the rest of the fields" $
            forAll (liftM3 (,,) arbitrary validCoordinates fieldStateMatrix) $
                \(TestFieldState newState, coords@(x, y), fields) ->
                    let newFields = replaceFieldState newState coords fields
                        newRow = newFields !! y
                    in take y newFields == take y fields &&
                        drop (y + 1) newFields == drop (y + 1) fields &&
                        take x newRow == take x (fields !! y) &&
                        drop (x + 1) newRow == drop (x + 1) (fields !! y)
        ]

    , testGroup "getRemainingShips Tests"
        [ testProperty "empty list returns empty list" $
            getRemainingShips [] == []
        , testProperty "ships get filtered correctly" $
            let aliveShips = [Ship Destroyer [] 3, Ship Carrier [] 1]
                deadShips = [Ship Battleship [] 0, Ship Submarine [] (-1)]
            in getRemainingShips (aliveShips ++ deadShips) == aliveShips
        ]

    , testGroup "registerHit Tests"
        [ testProperty "hitting out of bounds returns Nothing" $
            forAll (liftM2 (,) arbitrary invalidCoordinates) $
                \(TestGameBoard gameBoard, coords) ->
                    case registerHit gameBoard coords of
                        (Nothing, _) -> True
                        (Just _, _) -> False
        , testProperty "hitting already hit field returns Nothing" $
            let fields = replicate 10 (replicate 10 Hit)
                gameBoard = GameBoard fields []
            in forAll validCoordinates $ \coords ->
                case registerHit gameBoard coords of
                    (Nothing, _) -> True
                    (Just _, _) -> False
        , testProperty "hitting unknown field without ship results in Miss" $
            let fields = replicate 10 (replicate 10 Unknown)
                gameBoard = GameBoard fields []
            in forAll validCoordinates $ \coords ->
                case registerHit gameBoard coords of
                    (Just (GameBoard newFields _), _) ->
                        getFieldState newFields coords == Miss
                    (Nothing, _) -> False
        , testProperty "hitting field with ship results in Hit" $
            let fields = replicate 10 (replicate 10 Unknown)
                gameBoard = GameBoard fields [Ship Destroyer [(x, y) | x <- [0..9], y <- [0..9]] 100]
            in forAll validCoordinates $ \coords ->
                case registerHit gameBoard coords of
                    (Just (GameBoard newFields _), _) ->
                        getFieldState newFields coords == Hit
                    (Nothing, _) -> False
        , testProperty "hitting ship 1 health ship correctly sinks it" $
            let fields = replicate 10 (replicate 10 Unknown)
                gameBoard = GameBoard fields [Ship Destroyer [(x, y) | x <- [0..9], y <- [0..9]] 1]
            in forAll validCoordinates $ \coords ->
                case registerHit gameBoard coords of
                    (Just (GameBoard newFields _), _) ->
                        newFields == (replicate 10 (replicate 10 Sunken))
                    (Nothing, _) -> False
        ]
    ]
