# PFL - Prolog Coursework

## Topic and Group

Group Name: Aquapipe_3

### Group Members

- António Lino dos Santos (201705558) - XX% Contribution
- Gabriel Tomaz Machado Júnior (202008860) - XX% Contribution
- Manuel Rivera Villatte (202401168) - XX% Contribution

### Topic (Game) and Rules

The topic, as stated in the group name, is Aqua Pipe.\
In this game, the player's mission is to make aqua pipe lines.

There 2 possible Game Modes:

#### AquaPipe 3-in-a-row (3x3 Board)

- Players: 2-players
- Board: 3x3 spaces
- Pieces: 3 straight pipes of each diameter (3 x 12mm, 3 x 16mm, 3 x 19mm) for each player, red and blue.

##### Winning Conditions (3x3)

- By making a row of 3 pipes with the same diameter and same color, horizontally, vertically or diagonally.

#### AquaPipe 4-in-a-row (4x4 Board)

- Players: 2-players
- Board: 4x4 spaces
- Pieces: 4 straight pipes of each diameter (4 x 12mm, 4 x 16mm, 4 x 19mm) for each player, red and blue.
          1 U-shaped of each diameter (1 x 12mm, 1 x 16mm)

##### Winning Conditions (4x4)

- By making 4 rows of 3 pipes, each row with the same diameter and same color.
- By making a row of 4 pipes with the same diameter and same color, horizontally, vertically or diagonally.

#### Rules

At the beginning of the game, there are no pieces on the board.
The players choose the colors and who is the first player by casting dice or coin toss.

During the game, the following rules apply:

- Players may place one of their own pipes at any available space on the board or move one of their own pipes to another available space.
- Player can only move a pipe on the board after he/she placed at least one each of the three size pipes.
- Three different size pipes can be placed on the same space because of their structure.
- **Only in 4-in-a-row**: U-shaped pipes can be placed on two horizontal or two vertical spaces.

#### Sources of Game Rules

[Game Page on Kickstarter](https://www.kickstarter.com/projects/logygames/aqua-pipe)\
[Game Page on BoardGameGeek](https://boardgamegeek.com/boardgame/414235/aqua-pipe)

## Game Logic

### Game Configuration Representation

**Mode-Players-Difficulty** where,

- *Mode* - One of the 2 Game Modes of AquaPipe: 3x3 or 4x4
- *Players (or F/S)* - Can be h/h, h/pc, pc/h, pc/pc, where h -> Human, pc -> Computer
- *Difficulty* - represents the difficulty of the PC, it can be Random, Greedy or Minimax

### Internal Game State Representation

**Mode-F/S-Difficulty-Board-P-Color** where,

- *Mode* - One of the 2 Game Modes of AquaPipe: 3x3 or 4x4
- *F* - First Player (h or pc) with color blue
- *S* - Second Player (h or pc) with color red
- *Difficulty* - represents the difficulty of the PC, it can be Random, Greedy or Minimax
- *Board* - Bi-dimensional list of 3x3 or 4x4 size, depends on Game Mode
- *P* - Player to play on the current turn (F on the first turn)
