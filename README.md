# BLINQ - PFL Project

**Blinq (Binary + link)** is a connection game that uses dual coloured square blocks composed of two rectangles and shared by both players.

Players alternate turns placing those blocks according to some simple stacking rules, each trying to connect the two sides of the board of her colour. Notice that on their turn, players play both colours at once, as the blocks are indivisible.

## Project Members

- Bruno Miguel Gonçalves Ferreira | up202207863@edu.fe.up.pt
- Pedro Miguel Silva Roleira Marinho | up202206854@edu.fe.up.pt

## Participation

- Each one of us did **50%** of the asked work. The `value` and `find_path` functions were made by both students and the rest of the functions were divided accordingly.

## Installation and Execution

1. Ensure that **SICStus Prolog 4.9** is installed.
2. Clone the repository or download the project files.
3. Open a terminal in the project directory.
4. Run the following command to load the game files:

```bash
sicstus -l src/game.pl
```

5. Finally execute the command bellow to start the game:

```bash
play.
```

## Description of the game

**Blinq (short for "Binary + Link")** is a two-player connection game. The game is played on a 5x5 board using dual-colored square blocks. Each block is composed of two rectangles, and both players share the same pieces. The objective is to connect the two opposite sides of the board corresponding to a player’s color. The game requires strategic placement and stacking of blocks to create a continuous path while preventing the opponent from completing theirs.

### Rules

1. Place the neutral block in the center space of the board (or any cell as a variant).
2. Each player is assigned a color (white or black) and receives half of the blocks.
3. Starting with White, players alternate turns placing one block on the board.
4. Blocks must be placed:

   - Parallel to the board lines.
   - Within a cell on the board when on the ground level.
   - On a 2x2 arrangement of blocks when placed on higher levels, centered on it.

5. The game ends when a player connects their two sides or resigns. If all pieces are used without a connection, the game ends in a draw.

### References _TODO CHANGE THIS LATER_

Blinq rules, design, and rulebook by Néstor Romeral Andrés, 2017.

## Considerations for game extensions

- **Visual indication of valid moves:** Displaying clearly which moves are possible for the player to make.

- **Move choice via keyboard or coordinates input:** Players can either choose their move using the keyboard or by entering the coordinates of the desired position. This provides flexibility in how players interact with the game.

- **Computer player with two difficulty levels:**
  1. **Easy (1):** The computer randomly selects its moves.
  2. **Hard (2):** The computer evaluates the game state and selects the best possible move based on greedy strategy.

## Game Logic

### Game Configuration Representation

_TODO TODO TODO TODO TODO_

_TODO TODO TODO TODO TODO_

_TODO TODO TODO TODO TODO_

_Describe the information required to represent the game configuration, how it is represented internally and how it is used by the initial_state/2 predicate._

_TODO TODO TODO TODO TODO_

_TODO TODO TODO TODO TODO_

_TODO TODO TODO TODO TODO_

### Internal Game State Representation

- **Current board state:** The overall layout of the game board, including the positions of all pieces.
- **Player turn:** Indicates which player's turn it is (White or Black).
- **Number of white blocks left:** The remaining white blocks available for the player.
- **Number of black blocks left:** The remaining black blocks available for the player.
- **Current valid moves:** A list of all valid moves the current player can make.

#### Internal Representation:

- **White Player block:** Represented as `white`.
- **Black Player block:** Represented as `black`.
- **Block orientation:** Can be `1` or `2`, representing different orientations of a block.
- **Block size:** Each block has a size of `2x2` pieces.
- **Empty spaces:** Represented as `empty`, indicating locations on the board that are not occupied by any block or piece.
- **White Player pieces:** Represented as `white`, referring to the pieces controlled by the White Player.
- **Black Player pieces:** Represented as `black`, referring to the pieces controlled by the Black Player.
- **Piece height:** Each piece, in addition to its color, has a field for the **height**, which allows for stacking representation (i.e., indicating how many pieces are stacked in a particular location).

#### Example representations:

- Initial State:

```
[
[[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0], [empty,0],[empty,0]],
[[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0], [empty,0],[empty,0]],
[[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0], [empty,0],[empty,0]],
[[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0], [empty,0],[empty,0]],
[[empty,0],[empty,0],[empty,0],[empty,0],[white,1],[black,1],[empty,0],[empty,0], [empty,0],[empty,0]],
[[empty,0],[empty,0],[empty,0],[empty,0],[black,1],[white,1],[empty,0],[empty,0], [empty,0],[empty,0]],
[[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0], [empty,0],[empty,0]],
[[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0], [empty,0],[empty,0]],
[[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0], [empty,0],[empty,0]],
[[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0], [empty,0],[empty,0]]
]
```

![Initial State](docs/initial_state.png)

- Intermediate State:

```
[
[[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[white,1],[white,1],[empty,0],[empty,0]],
[[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[black,1],[black,1],[empty,0],[empty,0]],
[[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[white,1],[black,1],[empty,0],[empty,0]],
[[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[white,1],[black,1],[empty,0],[empty,0]],
[[empty,0],[empty,0],[empty,0],[empty,0],[white,1],[black,1],[white,1],[black,1],[empty,0],[empty,0]],
[[empty,0],[empty,0],[empty,0],[empty,0],[black,1],[white,1],[white,1],[black,1],[empty,0],[empty,0]],
[[white,1],[white,1],[white,1],[white,1],[black,1],[white,1],[empty,0],[empty,0],[empty,0],[empty,0]],
[[black,1],[black,1],[black,1],[black,1],[black,1],[white,1],[empty,0],[empty,0],[empty,0],[empty,0]],
[[white,1],[white,1],[empty,0],[empty,0],[black,1],[white,1],[empty,0],[empty,0],[empty,0],[empty,0]],
[[black,1],[black,1],[empty,0],[empty,0],[black,1],[white,1],[empty,0],[empty,0],[empty,0],[empty,0]]
]
```

![Intermediate State](docs/intermediate_state.png)

- Final State:

```
[
[[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[white,1],[white,1],[empty,0],[empty,0]],
[[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[empty,0],[black,1],[black,1],[empty,0],[empty,0]],
[[white,1],[black,1],[empty,0],[empty,0],[empty,0],[empty,0],[white,1],[black,1],[empty,0],[empty,0]],
[[white,1],[black,1],[empty,0],[empty,0],[empty,0],[empty,0],[white,1],[black,1],[empty,0],[empty,0]],
[[white,1],[black,1],[white,1],[white,1],[white,1],[black,1],[white,1],[black,1],[empty,0],[empty,0]],
[[white,1],[white,2],[white,2],[white,2],[black,2],[white,2],[white,2],[black,1],[empty,0],[empty,0]],
[[white,1],[black,2],[white,3],[white,3],[black,2],[black,2],[black,2],[white,1],[empty,0],[empty,0]],
[[black,1],[black,2],[black,3],[black,3],[black,2],[white,2],[black,2],[black,1],[empty,0],[empty,0]],
[[white,1],[black,2],[white,2],[white,2],[black,2],[white,2],[black,2],[white,1],[white,1],[white,1]],
[[black,1],[black,1],[white,1],[black,1],[black,1],[white,1],[black,1],[black,1],[black,1],[black,1]]
]
```

![Final State](docs/final_state.png)

### Move Representation

- **Information Required:**

  - **Coordinates (row, column):** Represent the position of the move on the board. The coordinates indicate the location of the **lower-left corner** of the block being moved.
  - **Move type:** Specifies the orientation in which the block is moved (1 or 2).

- **Internal Representation:** [Row, Col, Direction]

  - White Block with direction 1:

    ![White Block - Direction 1](docs/white_direction_1.png)

  - White Block with direction 2:

    ![White Block - Direction 2](docs/white_direction_2.png)

  - Black Block with direction 1:

    ![Black Block - Direction 1](docs/black_direction_1.png)

  - Black Block with direction 2:

    ![Black Block - Direction 2](docs/black_direction_2.png)

- **Usage:** The move/3 predicate applies a move and returns the new game state.

### User Interaction

_TODO TODO TODO TODO TODO_

_TODO TODO TODO TODO TODO_

_TODO TODO TODO TODO TODO_

Briefly describe the game menu system, as well as how interaction with the user is performed, focusing on input validation (e.g., when reading a move).

- **Game Menu System:** Text-based menu with options for starting a new game, viewing rules, and exiting.

- **Input Validation:** Ensures valid moves by checking the board state and move legality. Prompts the user to re-enter invalid inputs.

_TODO TODO TODO TODO TODO_

_TODO TODO TODO TODO TODO_

_TODO TODO TODO TODO TODO_

## Conclusions

### Summary _TODO TODO TODO TODO REVIEW_

The development of **Blinq** allowed exploration of **Prolog**'s capabilities for game logic implementation. The program effectively enforces the rules, and supports player interactions.

### Limitations

- No support for multiplayer over a network.
- As the game progresses the computer player gets slower.

### Future Developments

- Improve computer AI.
- Develop an improved GUI for better user experience.
- Extend gameplay to include more complex rules and mechanics.
- Variable board size.

## Bibliography _TODO CHANGE THIS LATER_

Blinq rules, design, and rulebook by Néstor Romeral Andrés, 2017.
