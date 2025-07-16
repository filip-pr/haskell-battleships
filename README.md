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

### Implementation parts

#### Game state representation

Implementation of game rules is pretty straight forward. Game state is basically completely given by two game boards, each with the states of all the fields (on the 10x10 play grid) and a list of ships on it, each with the fields they occupy. So game board is implemented as a tuple of 10x10 field state matrix and a list of ships. Each ship containing it's type (represented as an enum), fields coordinates (represented as a tuple of Ints) it occupies and it's remaining hit points. The pair of game boards is then modified according to the player/computer moves and the game rules. No non Prelude data types were used, although they could make some of the checks and update rules more efficient, the game board is only has 100 fields so even full copy/traverse is more than fast enough.

#### Game update rules

The game update rules are all pretty straight forward, they are all represented by simple validity check and update functions. The only problems that needed to be solved were the fact that not every move is valid and that there sometimes are more than one thing that the update should return (ie. the updated game board, and whether the move resulted in a miss, hit or sinking, and if so, of what). The first one is simply solved by having helper validity check function and the usage of Either or Maybe, the second one by using tuples.

#### Player input handling

Same as with the update rules where not every move is valid, neither is every player input, because of that, validity of each input is checked iteratively, first if it fits the basic format, then if it has valid values and lastly if those values make for a valid move. If any of these checks fail, the reason of failure is printed to the console and the input prompt is repeated.

#### Game output representation

Since the game is console based, the ways to give the player feedback are limited to text. Because of that all the output is either written text (for describing what happened or what is wrong with the move/input) or by ASCII art (for the board and remaining ship visualization).

#### Opponent decision making

Since battleships is mostly a guessing game, the strategy the opponent uses for decision making can't be too complex.

When it comes to ship placement, there isn't really any other optimal strategy other than just placing your ships in a way your opponent won't predict, meaning the best thing to do is to place them randomly. That is done by iteratively randomly trying to place the defined ship types, if they don't collide with already present ships or are not (even partially) outside the board we place them, otherwise we retry. Since the board size is relatively large compared to the number (and size) of the ships, there cannot be a situation in which greedy ship placement results in a deadlock, so the strategy of re/trying works well enough.

The arguably more interesting part of opponent decision making is the choice of next target. If no known hits are on the board, the best we can do is guess randomly (similarly to ship placement). On the other side, if we have any hits (that are not a sunken ship yet) we can do much more than that. In that case the decision not only depends on what positions have we already hit, but also what ships are still remaining. The decision process can be split into 4 steps, finding of possible targets, guessing what and where the ship is, checking whether it's placement would describe the hits and misses already present and randomly choosing a coordinate of the ship. If the 3rd step check succeeds, we randomly choose one of the ship's coordinates (that are still unknown and at most 1 field away from already hit field) and use them as our move, in the other case, we retry from the 2nd step and continue. This strategy is pretty close to optimal (as far as I can tell), only improvement that comes to my mind is the elimination of fields 1 away from any sunken ship from the ship placement pool. This was not implemented to give the player a slight strategic advantage (most definitely not because I forgot to add the check in first place). As with the ship placement, this strategy can't create a deadlock because it never eliminates any possible ship placements from the guess pool and because of that is guaranteed to guess a plausible solution in not too many guesses.


### Implementation structure

The implementation is organized into three main modules:

#### 1. `GameUtils.hs` - Core Data Structures and Utilities

**Data Types:**
- `Coordinates`: Represents coordinates on the field
- `FieldState`: Enum for (visible) states of fields on a game board
- `ShipOrientation`: Enum for ship orientation (horizontal/vertical)
- `ShipType`: Enum for types of ships
- `Ship`: Represents a ship, containing it's type, fields and remaining health
- `GameBoard`: Represents the state of one of the player's fleet

**Key Functions:**
- `createShip`: Creates ships of different types with proper coordinates
- `addShip`: Either adds a ship to the game board or returns a string containing what's wrong with the placement
- `registerHit`: Returns the result of the action and a game board after it if the action was valid
- `showBoardInformation`: Renders the game board with appropriate visibility

#### 2. `GameLogic.hs` - Game Flow and AI Logic

**Key Functions:**
- `placeShipsRandomly`: Randomly places a ships on a given board
- `placeOpponentShips`: Initializes opponents board (by placing them randomly)
- `placePlayersShips`: Prompts the player for ship placement and adds them to the board
- `performOpponentAction`: Performs the player's action and displays the result
- `performPlayerAction`: Prompts the player for action and displays the result
- `progressGame`: Manages turn-based gameplay loop
- `startGame`: Main game entry point and setup

#### 3. `Main.hs` - Application Entry Point
Simple entry point that calls `startGame` from `GameLogic`.


### Building and running the game

- For build only:
    ```
    cabal build
    ```

- For build and running:
    ```
    cabal run
    ```

- For REPL:
    ```
    cabal repl
    ```

### Running the tests

```
cabal configure --enable-tests
cabal test
```
