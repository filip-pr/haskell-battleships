# Battleships Game

A console-based implementation of the Battleships game written in Haskell.

## User Documentation

### Overview
This is a turn-based strategy game where you play against a computer opponent. The goal is to sink all of your opponent's ships before they sink yours. Each player has a 10x10 grid where they place their ships and try to guess the location of their opponent's ships.

### How to Play

#### Getting Started
1. **Build the game**: Use `cabal build` to compile the project
2. **Run the game**: Execute `cabal run` to start playing
3. **Follow the prompts**: The game will guide you through setup and gameplay

#### Ship Placement
When the game starts, you'll be asked to place your ships on the board:

**Option 1: Manual Placement**
- Choose 'm' for manual placement
- For each ship, enter coordinates in the format: `<row> <column> <orientation>`
- **Row**: A letter from A-J
- **Column**: A number from 1-10
- **Orientation**: 'H' for horizontal, 'V' for vertical
- **Example**: `A 3 H` places a ship at row A, column 3, horizontally

**Option 2: Random Placement**
- Choose 'r' for random placement
- All ships will be placed automatically
- During manual placement, you can also type `random` for any individual ship

#### Ship Types
The game includes the following ships:
- **Carrier (4x2)**: 8 hit points, largest ship
- **Battleship (4x1)**: 4 hit points, long and narrow
- **Submarine (2x2)**: 4 hit points, square-shaped
- **Destroyer (3x1)**: 3 hit points, medium-sized
- **Patrol Boat (2x1)**: 2 hit points, smallest ship (you get 2 of these)

#### Making Attacks
During your turn:
1. Enter target coordinates in the format: `<row> <column>`
2. **Example**: `B 5` attacks row B, column 5
3. The game will tell you if it was a hit, miss, or if you hit and sunk a ship

#### Game Symbols
- **' '** (space): Unknown/unexplored area
- **'#'**: Your ship (only visible on your board)
- **'O'**: Miss
- **'X'**: Hit
- **'S'**: Sunken ship

#### Winning
- **Victory**: Sink all opponent's ships first
- **Defeat**: All your ships are sunk

### Example Game Session

#### Manual ship placement

```
Do you want to place your ships manually 'm' or randomly 'r' ?: m

Place your Carrier (4x2): D 5 h

Current board state:

hint: Unknown: ' ', Ship: '#', Miss: 'O', Hit: 'X', Sunken: 'S'

     1   2   3   4   5   6   7   8   9   10
   +---+---+---+---+---+---+---+---+---+---+
 A |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 B |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 C |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 D |   |   |   |   | # | # | # | # |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 E |   |   |   |   | # | # | # | # |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 F |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 G |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 H |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 I |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 J |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+

Type 'y' to confirm the placement or 'n' to retry: y

Place your Battleship (4x1): random

Current board state:

hint: Unknown: ' ', Ship: '#', Miss: 'O', Hit: 'X', Sunken: 'S'

     1   2   3   4   5   6   7   8   9   10
   +---+---+---+---+---+---+---+---+---+---+
 A |   |   |   | # | # | # | # |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 B |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 C |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 D |   |   |   |   | # | # | # | # |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 E |   |   |   |   | # | # | # | # |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 F |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 G |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 H |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 I |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 J |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+

Type 'y' to confirm the placement or 'n' to retry: n

Place your Battleship (4x1): random

Current board state:

hint: Unknown: ' ', Ship: '#', Miss: 'O', Hit: 'X', Sunken: 'S'

     1   2   3   4   5   6   7   8   9   10
   +---+---+---+---+---+---+---+---+---+---+
 A |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 B |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 C |   |   |   |   |   |   |   |   |   | # |
   +---+---+---+---+---+---+---+---+---+---+
 D |   |   |   |   | # | # | # | # |   | # |
   +---+---+---+---+---+---+---+---+---+---+
 E |   |   |   |   | # | # | # | # |   | # |
   +---+---+---+---+---+---+---+---+---+---+
 F |   |   |   |   |   |   |   |   |   | # |
   +---+---+---+---+---+---+---+---+---+---+
 G |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 H |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 I |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 J |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+

Type 'y' to confirm the placement or 'n' to retry: y

Place your Submarine (2x2): H 2 h

Current board state:

hint: Unknown: ' ', Ship: '#', Miss: 'O', Hit: 'X', Sunken: 'S'

     1   2   3   4   5   6   7   8   9   10
   +---+---+---+---+---+---+---+---+---+---+
 A |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 B |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 C |   |   |   |   |   |   |   |   |   | # |
   +---+---+---+---+---+---+---+---+---+---+
 D |   |   |   |   | # | # | # | # |   | # |
   +---+---+---+---+---+---+---+---+---+---+
 E |   |   |   |   | # | # | # | # |   | # |
   +---+---+---+---+---+---+---+---+---+---+
 F |   |   |   |   |   |   |   |   |   | # |
   +---+---+---+---+---+---+---+---+---+---+
 G |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 H |   | # | # |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 I |   | # | # |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 J |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+

Type 'y' to confirm the placement or 'n' to retry: y

Place your Destroyer (3x1):
...
(the whole ship placement and game would be too long)
```

#### Random ship placement and gameplay
```
Do you want to place your ships manually 'm' or randomly 'r' ?: r

Your board:

hint: Unknown: ' ', Ship: '#', Miss: 'O', Hit: 'X', Sunken: 'S'

     1   2   3   4   5   6   7   8   9   10
   +---+---+---+---+---+---+---+---+---+---+
 A |   |   | # | # |   |   |   |   | # | # |
   +---+---+---+---+---+---+---+---+---+---+
 B | # |   |   |   |   |   |   |   | # | # |
   +---+---+---+---+---+---+---+---+---+---+
 C | # |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 D |   |   |   |   |   |   |   |   | # |   |
   +---+---+---+---+---+---+---+---+---+---+
 E |   |   |   |   |   |   |   |   | # |   |
   +---+---+---+---+---+---+---+---+---+---+
 F |   | # | # | # | # |   |   |   | # |   |
   +---+---+---+---+---+---+---+---+---+---+
 G |   | # | # | # | # |   |   |   | # |   |
   +---+---+---+---+---+---+---+---+---+---+
 H |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 I |   |   |   |   |   | # | # | # |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 J |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+

Your turn!

Enter your attack coordinates: D 7
Opponent's updated board:

Remaining ships:

-Patrol Boat: ##

-Patrol Boat: ##

-Destroyer:   ###

-Submarine:   ##
              ##

-Battleship:  ####

-Carrier:     ####
              ####

hint: Unknown: ' ', Miss: 'O', Hit: 'X', Sunken: 'S'

     1   2   3   4   5   6   7   8   9   10
   +---+---+---+---+---+---+---+---+---+---+
 A |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 B |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 C |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 D |   |   |   |   |   |   | X |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 E |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 F |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 G |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 H |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 I |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 J |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+

Your attack was a Hit!

Opponent's turn! (press Enter to progress)


Your updated board:

hint: Unknown: ' ', Ship: '#', Miss: 'O', Hit: 'X', Sunken: 'S'

     1   2   3   4   5   6   7   8   9   10
   +---+---+---+---+---+---+---+---+---+---+
 A |   |   | # | X |   |   |   |   | # | # |
   +---+---+---+---+---+---+---+---+---+---+
 B | # |   |   |   |   |   |   |   | # | # |
   +---+---+---+---+---+---+---+---+---+---+
 C | # |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 D |   |   |   |   |   |   |   |   | # |   |
   +---+---+---+---+---+---+---+---+---+---+
 E |   |   |   |   |   |   |   |   | # |   |
   +---+---+---+---+---+---+---+---+---+---+
 F |   | # | # | # | # |   |   |   | # |   |
   +---+---+---+---+---+---+---+---+---+---+
 G |   | # | # | # | # |   |   |   | # |   |
   +---+---+---+---+---+---+---+---+---+---+
 H |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 I |   |   |   |   |   | # | # | # |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 J |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+

Opponent attacked A 4.

Opponents attack was a Hit!

Your turn!

Enter your attack coordinates: D 6
Opponent's updated board:

Remaining ships:

-Patrol Boat: ##

-Patrol Boat: ##

-Destroyer:   ###

-Submarine:   ##
              ##

-Battleship:  ####

-Carrier:     ####
              ####

hint: Unknown: ' ', Miss: 'O', Hit: 'X', Sunken: 'S'

     1   2   3   4   5   6   7   8   9   10
   +---+---+---+---+---+---+---+---+---+---+
 A |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 B |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 C |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 D |   |   |   |   |   | X | X |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 E |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 F |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 G |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 H |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 I |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 J |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+

Your attack was a Hit!

Opponent's turn! (press Enter to progress)


Your updated board:

hint: Unknown: ' ', Ship: '#', Miss: 'O', Hit: 'X', Sunken: 'S'

     1   2   3   4   5   6   7   8   9   10
   +---+---+---+---+---+---+---+---+---+---+
 A |   |   | S | S |   |   |   |   | # | # |
   +---+---+---+---+---+---+---+---+---+---+
 B | # |   |   |   |   |   |   |   | # | # |
   +---+---+---+---+---+---+---+---+---+---+
 C | # |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 D |   |   |   |   |   |   |   |   | # |   |
   +---+---+---+---+---+---+---+---+---+---+
 E |   |   |   |   |   |   |   |   | # |   |
   +---+---+---+---+---+---+---+---+---+---+
 F |   | # | # | # | # |   |   |   | # |   |
   +---+---+---+---+---+---+---+---+---+---+
 G |   | # | # | # | # |   |   |   | # |   |
   +---+---+---+---+---+---+---+---+---+---+
 H |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 I |   |   |   |   |   | # | # | # |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 J |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+

Opponent attacked A 3.

Opponents attack was a Hit!
Patrol Boat (2x1) sunken!

Your turn!

Enter your attack coordinates: D 8
Opponent's updated board:

Remaining ships:

-Patrol Boat: ##

-Patrol Boat: ##

-Destroyer:   ###

-Submarine:   ##
              ##

-Battleship:  ####

-Carrier:     ####
              ####

hint: Unknown: ' ', Miss: 'O', Hit: 'X', Sunken: 'S'

     1   2   3   4   5   6   7   8   9   10
   +---+---+---+---+---+---+---+---+---+---+
 A |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 B |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 C |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 D |   |   |   |   |   | X | X | O |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 E |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 F |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 G |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 H |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 I |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 J |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+

Your attack was a Miss!

Opponent's turn! (press Enter to progress)


Your updated board:

hint: Unknown: ' ', Ship: '#', Miss: 'O', Hit: 'X', Sunken: 'S'

     1   2   3   4   5   6   7   8   9   10
   +---+---+---+---+---+---+---+---+---+---+
 A |   |   | S | S |   |   |   |   | # | # |
   +---+---+---+---+---+---+---+---+---+---+
 B | # |   |   |   |   |   |   |   | # | # |
   +---+---+---+---+---+---+---+---+---+---+
 C | # |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 D |   |   |   |   |   |   |   |   | # |   |
   +---+---+---+---+---+---+---+---+---+---+
 E |   |   |   |   |   |   |   |   | # |   |
   +---+---+---+---+---+---+---+---+---+---+
 F |   | # | # | # | # |   |   |   | # |   |
   +---+---+---+---+---+---+---+---+---+---+
 G |   | # | # | # | # |   |   |   | # |   |
   +---+---+---+---+---+---+---+---+---+---+
 H |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+
 I |   |   |   |   |   | # | # | # | O |   |
   +---+---+---+---+---+---+---+---+---+---+
 J |   |   |   |   |   |   |   |   |   |   |
   +---+---+---+---+---+---+---+---+---+---+

Opponent attacked I 9.

Opponents attack was a Miss!

...
(the whole game would be too long)
```

## Developer Documentation

- TODO
